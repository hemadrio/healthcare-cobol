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
      *             - COBRA-NTFY-FILE (COBRA QUALIFYING EVENTS)
      *             - ACA-RECON-FILE  (ACA EXCHANGE RECONCILIATION)
      *             - ACCUM-RST-FILE  (ACCUMULATOR RESET CONTROL)
      *             - SYBASE ELIGIBILITY / ACCUMULATOR TABLES
      *
      * OUTPUT:     - ELIG-271-FILE   (271 RESPONSE FILE)
      *             - ENRL-RPT-FILE   (ENROLLMENT REPORT)
      *             - COBRA-LTR-FILE  (COBRA NOTIFICATION LETTERS)
      *             - ACA-RECON-OUT   (ACA RECONCILIATION OUTPUT)
      *             - ACCUM-RPT-FILE  (ACCUMULATOR RESET REPORT)
      *             - RECOUP-FILE     (RECOUPMENT QUEUE FILE)
      *             - AUDIT-TRAIL-FILE (CHANGE HISTORY LOG)
      *             - ERROR-FILE      (REJECTED TRANSACTIONS)
      *             - SYBASE TABLE UPDATES
      *
      * TABLES:     T_MEMBER_ELIG, T_MEMBER_ACCUM, T_FAMILY_ACCUM,
      *             T_PLAN_BENEFIT, T_ENRL_HISTORY, T_AUDIT_TRAIL,
      *             T_COBRA_TRACK, T_ACA_EXCHANGE, T_APTC_DETAIL,
      *             T_VISIT_LIMITS, T_SPECIAL_PROGRAMS, T_PCP_PANEL,
      *             T_RETRO_QUEUE, T_WAITING_PERIOD, T_PREEXIST,
      *             T_COB_OTHER_INS, T_STATE_MEDICAID_CAT
      *
      * ABEND CODES:
      *   U0810 - UNABLE TO OPEN INPUT FILE
      *   U0811 - UNABLE TO OPEN OUTPUT FILE
      *   U0820 - DATABASE CONNECTION FAILURE
      *   U0830 - CRITICAL DATA INTEGRITY ERROR
      *   U0840 - ACCUMULATOR OVERFLOW DETECTED
      *   U0850 - PLAN CONFIGURATION MISSING
      *   U0860 - DEADLOCK MAX RETRIES EXCEEDED
      *   U0870 - SEQUENCE NUMBER GENERATION FAILED
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
      * 2006-04-10 TLIU       HC-02980   VISIT LIMIT TRACKING
      * 2007-03-22 TLIU       HC-03190   MENTAL HEALTH PARITY ACT
      * 2007-09-15 SWRIGHT    HC-03290   PCP ASSIGNMENT LOGIC
      * 2008-02-01 KMENDEZ    HC-03355   WAITING PERIOD CALCULATION
      * 2008-09-10 SWRIGHT    HC-03455   COBRA SUBSIDY (ARRA 2009)
      * 2009-06-20 PGUPTA     HC-03610   PRE-EXISTING CONDITION TRACK
      * 2010-07-01 SWRIGHT    HC-03920   DEPENDENT TO AGE 26 (ACA)
      * 2011-01-15 PGUPTA     HC-04105   GRANDFATHERED PLAN FLAG
      * 2011-08-30 TLIU       HC-04280   SPECIAL PROGRAMS ENROLLMENT
      * 2012-03-15 KMENDEZ    HC-04450   DISABLED DEPENDENT EXTENSION
      * 2012-11-01 SWRIGHT    HC-04620   COB OTHER INSURANCE TRACKING
      * 2013-10-01 PGUPTA     HC-04780   ACA EXCHANGE ENROLLMENT
      * 2014-01-02 PGUPTA     HC-04912   APTC / CSR TRACKING
      * 2014-03-31 JMCCARTHY  HC-05010   METAL TIER VALIDATION
      * 2014-09-15 PGUPTA     HC-05188   EHB VERIFICATION LOGIC
      * 2015-06-20 SWRIGHT    HC-05402   GRACE PERIOD PROCESSING
      * 2016-02-14 TLIU       HC-05690   FAMILY GLITCH FIX
      * 2016-09-01 KMENDEZ    HC-05820   NEWBORN AUTO-ENROLLMENT
      * 2017-04-15 PGUPTA     HC-05950   QMCSO CUSTODY ORDER PROCESS
      * 2017-08-30 KMENDEZ    HC-06012   SILVER LOADING LOGIC
      * 2018-01-20 SWRIGHT    HC-06150   EMBEDDED DEDUCTIBLE LOGIC
      * 2018-07-10 TLIU       HC-06290   CONCURRENT ACCUM RESERVATION
      * 2019-04-01 PGUPTA     HC-06455   SHORT-TERM PLAN EXCLUSION
      * 2019-11-15 KMENDEZ    HC-06580   STATE MEDICAID CATEGORY UPD
      * 2020-03-15 SWRIGHT    HC-06780   COVID SPECIAL ENROLLMENT
      * 2020-08-01 PGUPTA     HC-06890   TELEHEALTH ELIG EXPANSION
      * 2021-01-10 PGUPTA     HC-07020   ENHANCED APTC (ARP ACT)
      * 2021-06-30 TLIU       HC-07155   CONTINUOUS ELIG (COVID PHE)
      * 2022-04-01 SWRIGHT    HC-07250   PHE UNWINDING PROCESSING
      * 2022-06-15 KMENDEZ    HC-07340   FIX RETRO ACCUM RECALC
      * 2023-03-01 PGUPTA     HC-07480   MEDICAID REDETERMINATION
      * 2023-09-01 SWRIGHT    HC-07612   ICD-10 ELIG RULE UPDATES
      * 2024-01-15 TLIU       HC-07780   IRA ENHANCED APTC EXTENSION
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

           SELECT COBRA-LTR-FILE
               ASSIGN TO COBRALTR
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-COBRA-LTR-STATUS.

           SELECT ACA-RECON-OUT
               ASSIGN TO ACARECON
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ACA-RECON-STATUS.

           SELECT ACCUM-RPT-FILE
               ASSIGN TO ACCUMRPT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ACCUM-RPT-STATUS.

           SELECT RECOUP-FILE
               ASSIGN TO RECOUPFL
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-RECOUP-STATUS.

           SELECT ERROR-FILE
               ASSIGN TO ERRFILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ERROR-STATUS.

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
               88  E834-REACTIVATION       VALUE '029'.
               88  E834-ADDRESS-CHANGE     VALUE '007'.
               88  E834-PCP-CHANGE         VALUE '032'.
               88  E834-NEWBORN-ADD        VALUE '050'.
               88  E834-QMCSO-ORDER        VALUE '055'.
               88  E834-DISABILITY-EXT     VALUE '060'.
           05  E834-MEMBER-SSN         PIC X(09).
           05  E834-MEMBER-ID          PIC X(12).
           05  E834-SUBSCRIBER-IND     PIC X(01).
               88  E834-IS-SUBSCRIBER      VALUE 'Y'.
               88  E834-IS-DEPENDENT       VALUE 'N'.
           05  E834-RELATION-CODE      PIC X(02).
               88  E834-REL-SELF           VALUE '18'.
               88  E834-REL-SPOUSE         VALUE '01'.
               88  E834-REL-CHILD          VALUE '19'.
               88  E834-REL-OTHER          VALUE '20'.
               88  E834-REL-DOMESTIC       VALUE '53'.
               88  E834-REL-FOSTER         VALUE '10'.
               88  E834-REL-WARD           VALUE '15'.
               88  E834-REL-STEPCHILD      VALUE '17'.
           05  E834-LAST-NAME          PIC X(30).
           05  E834-FIRST-NAME         PIC X(20).
           05  E834-MIDDLE-INIT        PIC X(01).
           05  E834-DOB                PIC X(08).
           05  E834-GENDER             PIC X(01).
               88  E834-MALE              VALUE 'M'.
               88  E834-FEMALE            VALUE 'F'.
               88  E834-UNKNOWN-GENDER    VALUE 'U'.
           05  E834-EFF-DATE           PIC X(08).
           05  E834-TERM-DATE          PIC X(08).
           05  E834-PLAN-CODE          PIC X(08).
           05  E834-PRIOR-PLAN-CODE    PIC X(08).
           05  E834-GROUP-NUMBER       PIC X(10).
           05  E834-DIVISION-CODE      PIC X(04).
           05  E834-CLASS-CODE         PIC X(04).
           05  E834-COVERAGE-LEVEL     PIC X(02).
               88  E834-COV-EE-ONLY       VALUE 'EO'.
               88  E834-COV-EE-SPOUSE     VALUE 'ES'.
               88  E834-COV-EE-CHILD      VALUE 'EC'.
               88  E834-COV-FAMILY        VALUE 'FA'.
           05  E834-COBRA-QUAL-EVENT   PIC X(02).
               88  E834-COBRA-TERM         VALUE '01'.
               88  E834-COBRA-HOURS-RED    VALUE '02'.
               88  E834-COBRA-DIVORCE      VALUE '03'.
               88  E834-COBRA-DEATH        VALUE '04'.
               88  E834-COBRA-MCARE-ELIG   VALUE '05'.
               88  E834-COBRA-DEP-LOSS     VALUE '06'.
               88  E834-COBRA-BANKRUPTCY   VALUE '07'.
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
               88  E834-SEP-MARRIAGE       VALUE 'MAR'.
               88  E834-SEP-BIRTH          VALUE 'BTH'.
               88  E834-SEP-ADOPTION       VALUE 'ADP'.
               88  E834-SEP-LOSS-COV       VALUE 'LOS'.
               88  E834-SEP-MOVE           VALUE 'MOV'.
               88  E834-SEP-COVID          VALUE 'COV'.
               88  E834-SEP-MEDICAID-LOSS  VALUE 'MLS'.
           05  E834-PCP-NPI            PIC X(10).
           05  E834-TERM-REASON-CODE   PIC X(03).
               88  E834-TERM-VOLUNTARY     VALUE '001'.
               88  E834-TERM-NONPAYMENT    VALUE '002'.
               88  E834-TERM-FRAUD         VALUE '003'.
               88  E834-TERM-DEATH         VALUE '004'.
               88  E834-TERM-INELIGIBLE    VALUE '005'.
               88  E834-TERM-EMPLOYER-TERM VALUE '006'.
               88  E834-TERM-AGE-OUT       VALUE '007'.
               88  E834-TERM-DIVORCE       VALUE '008'.
               88  E834-TERM-MOVE-OOA      VALUE '009'.
               88  E834-TERM-GROSS-MISC    VALUE '010'.
               88  E834-TERM-INCARCERATED  VALUE '011'.
               88  E834-TERM-OTHER         VALUE '099'.
           05  E834-DISABILITY-IND     PIC X(01).
               88  E834-IS-DISABLED        VALUE 'Y'.
           05  E834-DISABILITY-DT      PIC X(08).
           05  E834-STUDENT-IND        PIC X(01).
               88  E834-IS-STUDENT         VALUE 'Y'.
           05  E834-NEWBORN-MOTHER-ID  PIC X(12).
           05  E834-QMCSO-COURT-ORDER  PIC X(20).
           05  E834-ADDRESS-LINE-1     PIC X(35).
           05  E834-ADDRESS-LINE-2     PIC X(35).
           05  E834-CITY               PIC X(25).
           05  E834-STATE              PIC X(02).
           05  E834-ZIP                PIC X(09).
           05  E834-PHONE              PIC X(10).
           05  E834-EMAIL              PIC X(50).
           05  FILLER                  PIC X(40).

       FD  ELIG-270-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  ELIG-270-RECORD.
           05  E270-TRANS-ID           PIC X(10).
           05  E270-MEMBER-ID          PIC X(12).
           05  E270-SUBSCRIBER-ID      PIC X(12).
           05  E270-SERVICE-DATE       PIC X(08).
           05  E270-SERVICE-TYPE       PIC X(03).
               88  E270-SVC-HEALTH         VALUE '30 '.
               88  E270-SVC-MENTAL         VALUE '33 '.
               88  E270-SVC-SUBSTANCE      VALUE 'A7 '.
               88  E270-SVC-CHIRO          VALUE '35 '.
               88  E270-SVC-DENTAL         VALUE '36 '.
               88  E270-SVC-VISION         VALUE '47 '.
               88  E270-SVC-PHARMACY       VALUE '88 '.
               88  E270-SVC-HOSPITAL-IP    VALUE 'AG '.
               88  E270-SVC-HOSPITAL-OP    VALUE 'AJ '.
               88  E270-SVC-EMERGENCY      VALUE '86 '.
           05  E270-PROVIDER-NPI       PIC X(10).
           05  E270-PAYER-ID           PIC X(10).
           05  E270-PROCEDURE-CD       PIC X(05).
           05  E270-DIAGNOSIS-CD       PIC X(08).
           05  FILLER                  PIC X(222).

       FD  ELIG-271-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 600 CHARACTERS.
       01  ELIG-271-RECORD             PIC X(600).

       FD  ENRL-RPT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  ENRL-RPT-RECORD             PIC X(133).

       FD  COBRA-LTR-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS.
       01  COBRA-LTR-RECORD            PIC X(400).

       FD  ACA-RECON-OUT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  ACA-RECON-RECORD            PIC X(300).

       FD  ACCUM-RPT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.
       01  ACCUM-RPT-RECORD            PIC X(133).

       FD  RECOUP-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 200 CHARACTERS.
       01  RECOUP-RECORD.
           05  RQ-MEMBER-ID           PIC X(12).
           05  RQ-CLAIM-NUMBER        PIC X(15).
           05  RQ-CLAIM-DOS           PIC X(08).
           05  RQ-PAID-AMOUNT         PIC S9(09)V99 COMP-3.
           05  RQ-RECOUP-REASON       PIC X(03).
           05  RQ-ORIGINAL-PLAN       PIC X(08).
           05  RQ-NEW-PLAN            PIC X(08).
           05  RQ-RETRO-EFF-DATE      PIC X(08).
           05  RQ-RETRO-TERM-DATE     PIC X(08).
           05  RQ-QUEUE-DATE          PIC X(08).
           05  RQ-QUEUE-STATUS        PIC X(01).
           05  FILLER                 PIC X(112).

       FD  ERROR-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 300 CHARACTERS.
       01  ERROR-RECORD.
           05  ER-TRANS-TYPE           PIC X(03).
           05  ER-MEMBER-ID           PIC X(12).
           05  ER-ERROR-CODE          PIC X(06).
           05  ER-ERROR-MSG           PIC X(80).
           05  ER-SEVERITY            PIC X(01).
           05  ER-TIMESTAMP           PIC X(26).
           05  FILLER                 PIC X(172).

       FD  AUDIT-TRAIL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 350 CHARACTERS.
       01  AUDIT-TRAIL-RECORD          PIC X(350).

      *================================================================*
       WORKING-STORAGE SECTION.
      *================================================================*
       01  WS-PROGRAM-ID              PIC X(08)  VALUE 'HCELIGVR'.
       01  WS-VERSION                 PIC X(06)  VALUE '28.3.1'.
       01  ERR-ABEND-CODE             PIC 9(04)  VALUE ZEROES.

      *----------------------------------------------------------------*
      * FILE STATUS CODES
      *----------------------------------------------------------------*
       01  WS-FILE-STATUSES.
           05  WS-ENRL-834-STATUS     PIC X(02)  VALUE SPACES.
           05  WS-ELIG-270-STATUS     PIC X(02)  VALUE SPACES.
           05  WS-ELIG-271-STATUS     PIC X(02)  VALUE SPACES.
           05  WS-ENRL-RPT-STATUS     PIC X(02)  VALUE SPACES.
           05  WS-COBRA-LTR-STATUS    PIC X(02)  VALUE SPACES.
           05  WS-ACA-RECON-STATUS    PIC X(02)  VALUE SPACES.
           05  WS-ACCUM-RPT-STATUS    PIC X(02)  VALUE SPACES.
           05  WS-RECOUP-STATUS       PIC X(02)  VALUE SPACES.
           05  WS-ERROR-STATUS        PIC X(02)  VALUE SPACES.
           05  WS-AUDIT-STATUS        PIC X(02)  VALUE SPACES.

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
               88  WS-COBRA-INELIGIBLE           VALUE 'N'.
           05  WS-ACA-VALID-FLAG      PIC X(01)  VALUE 'N'.
               88  WS-ACA-VALID                  VALUE 'Y'.
               88  WS-ACA-INVALID                VALUE 'N'.
           05  WS-RETRO-FLAG          PIC X(01)  VALUE 'N'.
               88  WS-IS-RETRO                   VALUE 'Y'.
               88  WS-NOT-RETRO                  VALUE 'N'.
           05  WS-DB-ERROR-FLAG       PIC X(01)  VALUE 'N'.
               88  WS-DB-ERROR                   VALUE 'Y'.
           05  WS-PLAN-YEAR-RESET-FL  PIC X(01)  VALUE 'N'.
               88  WS-PLAN-YEAR-RESET            VALUE 'Y'.
           05  WS-GRACE-PERIOD-FLAG   PIC X(01)  VALUE 'N'.
               88  WS-IN-GRACE-PERIOD            VALUE 'Y'.
           05  WS-GRANDFATHERED-FLAG  PIC X(01)  VALUE 'N'.
               88  WS-IS-GRANDFATHERED           VALUE 'Y'.
           05  WS-EMBEDDED-DED-FLAG   PIC X(01)  VALUE 'N'.
               88  WS-HAS-EMBEDDED-DED           VALUE 'Y'.
           05  WS-PCP-REQUIRED-FLAG   PIC X(01)  VALUE 'N'.
               88  WS-PCP-REQUIRED               VALUE 'Y'.
           05  WS-PCP-VALID-FLAG      PIC X(01)  VALUE 'N'.
               88  WS-PCP-VALID                  VALUE 'Y'.
               88  WS-PCP-INVALID                VALUE 'N'.
           05  WS-WAITING-PERIOD-FL   PIC X(01)  VALUE 'N'.
               88  WS-IN-WAITING-PERIOD          VALUE 'Y'.
           05  WS-PREEXIST-FLAG       PIC X(01)  VALUE 'N'.
               88  WS-HAS-PREEXIST               VALUE 'Y'.
           05  WS-NEWBORN-FLAG        PIC X(01)  VALUE 'N'.
               88  WS-IS-NEWBORN                 VALUE 'Y'.
           05  WS-QMCSO-FLAG          PIC X(01)  VALUE 'N'.
               88  WS-HAS-QMCSO                  VALUE 'Y'.
           05  WS-COVID-PHE-FLAG      PIC X(01)  VALUE 'N'.
               88  WS-COVID-PHE-ACTIVE           VALUE 'Y'.
           05  WS-MH-PARITY-FLAG      PIC X(01)  VALUE 'N'.
               88  WS-MH-PARITY-APPLIES          VALUE 'Y'.
           05  WS-SELF-REFERRAL-FLAG  PIC X(01)  VALUE 'N'.
               88  WS-SELF-REFERRAL-OK           VALUE 'Y'.
           05  WS-DEDUCT-MET-FLAG     PIC X(01)  VALUE 'N'.
               88  WS-IND-DEDUCT-MET             VALUE 'Y'.
           05  WS-FAM-DEDUCT-MET-FL   PIC X(01)  VALUE 'N'.
               88  WS-FAM-DEDUCT-MET             VALUE 'Y'.
           05  WS-OOP-MET-FLAG        PIC X(01)  VALUE 'N'.
               88  WS-IND-OOP-MET                VALUE 'Y'.
           05  WS-FAM-OOP-MET-FLAG    PIC X(01)  VALUE 'N'.
               88  WS-FAM-OOP-MET                VALUE 'Y'.
           05  WS-LIFETIME-MAX-FLAG   PIC X(01)  VALUE 'N'.
               88  WS-LIFETIME-MAX-MET           VALUE 'Y'.
           05  WS-CASCADE-TERM-FLAG   PIC X(01)  VALUE 'N'.
               88  WS-CASCADE-TERM               VALUE 'Y'.
           05  WS-SSN-DUP-FLAG        PIC X(01)  VALUE 'N'.
               88  WS-SSN-IS-DUPLICATE           VALUE 'Y'.

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
           05  WS-REACTIVATION-CNT    PIC S9(09) COMP VALUE 0.
           05  WS-ADDRESS-CHG-CNT     PIC S9(09) COMP VALUE 0.
           05  WS-PCP-CHG-CNT         PIC S9(09) COMP VALUE 0.
           05  WS-NEWBORN-CNT         PIC S9(09) COMP VALUE 0.
           05  WS-QMCSO-CNT           PIC S9(09) COMP VALUE 0.
           05  WS-DISABILITY-CNT      PIC S9(09) COMP VALUE 0.
           05  WS-270-READ-CNT        PIC S9(09) COMP VALUE 0.
           05  WS-271-WRITE-CNT       PIC S9(09) COMP VALUE 0.
           05  WS-ACCUM-UPDATE-CNT    PIC S9(09) COMP VALUE 0.
           05  WS-ACCUM-RESET-CNT     PIC S9(09) COMP VALUE 0.
           05  WS-AUDIT-WRITE-CNT     PIC S9(09) COMP VALUE 0.
           05  WS-ACA-EXCHANGE-CNT    PIC S9(09) COMP VALUE 0.
           05  WS-COBRA-LTR-CNT       PIC S9(09) COMP VALUE 0.
           05  WS-RECOUP-CNT          PIC S9(09) COMP VALUE 0.
           05  WS-CASCADE-TERM-CNT    PIC S9(09) COMP VALUE 0.

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

       01  WS-CURR-DATE-8             PIC 9(08).
       01  WS-PLAN-YEAR-START         PIC 9(08)  VALUE ZEROES.
       01  WS-PLAN-YEAR-END           PIC 9(08)  VALUE ZEROES.
       01  WS-PLAN-YEAR-NEXT          PIC 9(08)  VALUE ZEROES.

       01  WS-DATE-WORK.
           05  WS-DATE-WORK-YYYY      PIC 9(04).
           05  WS-DATE-WORK-MM        PIC 9(02).
           05  WS-DATE-WORK-DD        PIC 9(02).

       01  WS-DATE-WORK-2.
           05  WS-DATE-WORK2-YYYY     PIC 9(04).
           05  WS-DATE-WORK2-MM       PIC 9(02).
           05  WS-DATE-WORK2-DD       PIC 9(02).

       01  WS-DATE-NUMERIC            PIC 9(08)  VALUE ZEROES.
       01  WS-DATE-NUMERIC-2          PIC 9(08)  VALUE ZEROES.
       01  WS-AGE-YEARS               PIC 9(03)  VALUE ZEROES.
       01  WS-AGE-MONTHS              PIC 9(04)  VALUE ZEROES.
       01  WS-DAYS-DIFF               PIC S9(05) COMP VALUE ZEROES.
       01  WS-MONTHS-DIFF             PIC S9(04) COMP VALUE ZEROES.
       01  WS-COBRA-MAX-MONTHS        PIC 9(02)  VALUE 18.
       01  WS-COBRA-END-DATE          PIC 9(08)  VALUE ZEROES.
       01  WS-COBRA-NOTIFY-DEADLINE   PIC 9(08)  VALUE ZEROES.
       01  WS-COBRA-ELECT-DEADLINE    PIC 9(08)  VALUE ZEROES.
       01  WS-DEPEND-AGE-LIMIT        PIC 9(02)  VALUE 26.
       01  WS-GRACE-PERIOD-DAYS       PIC 9(03)  VALUE 90.
       01  WS-WAITING-PERIOD-DAYS     PIC 9(03)  VALUE ZEROES.
       01  WS-FIRST-OF-MONTH-AFTER    PIC 9(08)  VALUE ZEROES.
       01  WS-RUNOUT-DAYS             PIC 9(03)  VALUE 90.
       01  WS-PREEXIST-LOOKBACK-MO    PIC 9(02)  VALUE 6.
       01  WS-PREEXIST-EXCLUSION-MO   PIC 9(02)  VALUE 12.
       01  WS-CREDITABLE-COV-MONTHS   PIC 9(02)  VALUE ZEROES.
       01  WS-SEP-WINDOW-DAYS         PIC 9(03)  VALUE 60.
       01  WS-INTEGER-DATE-1          PIC 9(07).
       01  WS-INTEGER-DATE-2          PIC 9(07).

      *----------------------------------------------------------------*
      * PLAN BENEFIT CONFIGURATION TABLE (LOADED FROM DB)
      *----------------------------------------------------------------*
       01  WS-PLAN-TABLE.
           05  WS-PLAN-TABLE-COUNT    PIC 9(03)  VALUE ZEROES.
           05  WS-PLAN-ENTRY OCCURS 50 TIMES.
               10  WS-PLN-PLAN-CODE   PIC X(08).
               10  WS-PLN-PLAN-TYPE   PIC X(03).
                   88  WS-PLN-HMO     VALUE 'HMO'.
                   88  WS-PLN-PPO     VALUE 'PPO'.
                   88  WS-PLN-POS     VALUE 'POS'.
                   88  WS-PLN-EPO     VALUE 'EPO'.
                   88  WS-PLN-HDHP    VALUE 'HDH'.
                   88  WS-PLN-IND     VALUE 'IND'.
                   88  WS-PLN-MAD     VALUE 'MAD'.
                   88  WS-PLN-MMC     VALUE 'MMC'.
               10  WS-PLN-IND-DED-IN  PIC S9(07)V99 COMP-3.
               10  WS-PLN-IND-DED-OON PIC S9(07)V99 COMP-3.
               10  WS-PLN-FAM-DED-IN  PIC S9(07)V99 COMP-3.
               10  WS-PLN-FAM-DED-OON PIC S9(07)V99 COMP-3.
               10  WS-PLN-EMBEDDED-FL PIC X(01).
               10  WS-PLN-EMBED-CNT   PIC 9(01).
               10  WS-PLN-IND-OOP-IN  PIC S9(07)V99 COMP-3.
               10  WS-PLN-IND-OOP-OON PIC S9(07)V99 COMP-3.
               10  WS-PLN-FAM-OOP-IN  PIC S9(07)V99 COMP-3.
               10  WS-PLN-FAM-OOP-OON PIC S9(07)V99 COMP-3.
               10  WS-PLN-LIFETIME-MX PIC S9(09)V99 COMP-3.
               10  WS-PLN-ANNUAL-MAX  PIC S9(09)V99 COMP-3.
               10  WS-PLN-COPAY-PCP   PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-SPEC  PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-ER    PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-URGENT PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-IP    PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-RX-GN PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-RX-BR PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-RX-SP PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-MH    PIC S9(05)V99 COMP-3.
               10  WS-PLN-COPAY-PT    PIC S9(05)V99 COMP-3.
               10  WS-PLN-COINSUR-IN  PIC S9(03)V99 COMP-3.
               10  WS-PLN-COINSUR-OON PIC S9(03)V99 COMP-3.
               10  WS-PLN-PCP-REQD    PIC X(01).
               10  WS-PLN-REFERRAL-RQ PIC X(01).
               10  WS-PLN-GRANDFATHRD PIC X(01).
               10  WS-PLN-WAIT-PERIOD PIC 9(03).
               10  WS-PLN-WAIT-TYPE   PIC X(01).
                   88  WS-PLN-WAIT-FOM VALUE 'F'.
                   88  WS-PLN-WAIT-CAL VALUE 'C'.
               10  WS-PLN-PREEXIST-FL PIC X(01).
               10  WS-PLN-YEAR-START  PIC X(04).
               10  WS-PLN-EHB-COMPL   PIC X(01).
               10  WS-PLN-METAL-TIER  PIC X(02).
               10  WS-PLN-AV-PCT      PIC 9(03).
               10  WS-PLN-NETWORK-CD  PIC X(06).
               10  WS-PLN-PT-VISITS   PIC 9(03).
               10  WS-PLN-OT-VISITS   PIC 9(03).
               10  WS-PLN-MH-VISITS   PIC 9(03).
               10  WS-PLN-CHIRO-VISIT PIC 9(03).
               10  WS-PLN-SPEECH-VIS  PIC 9(03).
               10  WS-PLN-SUD-VISITS  PIC 9(03).

      *----------------------------------------------------------------*
      * FAMILY MEMBER TABLE FOR ACCUMULATOR ROLLUP
      *----------------------------------------------------------------*
       01  WS-FAMILY-TABLE.
           05  WS-FAMILY-COUNT        PIC 9(02) VALUE ZEROES.
           05  WS-FAMILY-ENTRY OCCURS 15 TIMES.
               10  WS-FAM-MEMBER-ID   PIC X(12).
               10  WS-FAM-RELATION    PIC X(02).
               10  WS-FAM-DED-USED    PIC S9(07)V99 COMP-3.
               10  WS-FAM-OOP-USED    PIC S9(07)V99 COMP-3.
               10  WS-FAM-DED-MET-FL  PIC X(01).
               10  WS-FAM-AGE         PIC 9(03).

      *----------------------------------------------------------------*
      * COBRA QUALIFYING EVENT TABLE
      *----------------------------------------------------------------*
       01  WS-COBRA-DURATION-TABLE.
           05  FILLER PIC X(04) VALUE '0118'.
           05  FILLER PIC X(04) VALUE '0218'.
           05  FILLER PIC X(04) VALUE '0336'.
           05  FILLER PIC X(04) VALUE '0436'.
           05  FILLER PIC X(04) VALUE '0536'.
           05  FILLER PIC X(04) VALUE '0636'.
           05  FILLER PIC X(04) VALUE '0736'.
       01  WS-COBRA-DUR-TABLE-R REDEFINES WS-COBRA-DURATION-TABLE.
           05  WS-COBRA-DUR-ENTRY OCCURS 7 TIMES.
               10  WS-COBRA-DUR-EVENT PIC X(02).
               10  WS-COBRA-DUR-MONTHS PIC 9(02).

      *----------------------------------------------------------------*
      * VISIT LIMIT WORK AREA
      *----------------------------------------------------------------*
       01  WS-VISIT-LIMITS.
           05  WS-VL-PT-ALLOWED       PIC 9(03) VALUE ZEROES.
           05  WS-VL-PT-USED          PIC 9(03) VALUE ZEROES.
           05  WS-VL-PT-REMAIN        PIC 9(03) VALUE ZEROES.
           05  WS-VL-OT-ALLOWED       PIC 9(03) VALUE ZEROES.
           05  WS-VL-OT-USED          PIC 9(03) VALUE ZEROES.
           05  WS-VL-OT-REMAIN        PIC 9(03) VALUE ZEROES.
           05  WS-VL-MH-ALLOWED       PIC 9(03) VALUE ZEROES.
           05  WS-VL-MH-USED          PIC 9(03) VALUE ZEROES.
           05  WS-VL-MH-REMAIN        PIC 9(03) VALUE ZEROES.
           05  WS-VL-CHIRO-ALLOWED    PIC 9(03) VALUE ZEROES.
           05  WS-VL-CHIRO-USED       PIC 9(03) VALUE ZEROES.
           05  WS-VL-CHIRO-REMAIN     PIC 9(03) VALUE ZEROES.
           05  WS-VL-SPEECH-ALLOWED   PIC 9(03) VALUE ZEROES.
           05  WS-VL-SPEECH-USED      PIC 9(03) VALUE ZEROES.
           05  WS-VL-SPEECH-REMAIN    PIC 9(03) VALUE ZEROES.
           05  WS-VL-SUD-ALLOWED      PIC 9(03) VALUE ZEROES.
           05  WS-VL-SUD-USED         PIC 9(03) VALUE ZEROES.
           05  WS-VL-SUD-REMAIN       PIC 9(03) VALUE ZEROES.
           05  WS-VL-NOTIFY-PCT       PIC 9(03) VALUE 80.

      *----------------------------------------------------------------*
      * BENEFIT ACCUMULATOR WORK AREA
      *----------------------------------------------------------------*
       01  WS-ACCUM-WORK.
           05  WS-DEDUCTIBLE-IND-IN   PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-DEDUCTIBLE-IND-OON  PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-DEDUCTIBLE-FAM-IN   PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-DEDUCTIBLE-FAM-OON  PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-OOP-IND-IN          PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-OOP-IND-OON         PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-OOP-FAM-IN          PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-OOP-FAM-OON         PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-LIFETIME-MAX-USED   PIC S9(09)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-ANNUAL-MAX-USED     PIC S9(09)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-DED-IND-IN     PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-DED-IND-OON    PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-OOP-IND-IN     PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-OOP-IND-OON    PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-LIFETIME-MAX   PIC S9(09)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-PLAN-ANNUAL-MAX     PIC S9(09)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-FAM-DEDUCTIBLE-IN   PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-FAM-OOP-MAX-IN      PIC S9(07)V99 COMP-3
                                                  VALUE ZEROES.
           05  WS-CARRYOVER-CREDIT    PIC S9(07)V99 COMP-3
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

       01  WS-ACA-METAL-TABLE.
           05  FILLER PIC X(08) VALUE 'BR056064'.
           05  FILLER PIC X(08) VALUE 'SV066074'.
           05  FILLER PIC X(08) VALUE 'GD076084'.
           05  FILLER PIC X(08) VALUE 'PT086094'.
           05  FILLER PIC X(08) VALUE 'CA000057'.
       01  WS-ACA-METAL-TABLE-R REDEFINES WS-ACA-METAL-TABLE.
           05  WS-METAL-ENTRY OCCURS 5 TIMES.
               10  WS-METAL-CODE      PIC X(02).
               10  WS-METAL-AV-LOW    PIC 9(03).
               10  WS-METAL-AV-HIGH   PIC 9(03).

      *----------------------------------------------------------------*
      * ESSENTIAL HEALTH BENEFIT CATEGORIES
      *----------------------------------------------------------------*
       01  WS-EHB-TABLE.
           05  WS-EHB-CATEGORY OCCURS 10 TIMES.
               10  WS-EHB-CAT-CODE    PIC X(03).
               10  WS-EHB-CAT-DESC    PIC X(40).
               10  WS-EHB-CAT-COVERED PIC X(01).
       01  WS-EHB-CAT-CODES.
           05  FILLER PIC X(03) VALUE 'AMB'.
           05  FILLER PIC X(03) VALUE 'EMR'.
           05  FILLER PIC X(03) VALUE 'HOS'.
           05  FILLER PIC X(03) VALUE 'MAT'.
           05  FILLER PIC X(03) VALUE 'MHS'.
           05  FILLER PIC X(03) VALUE 'RXD'.
           05  FILLER PIC X(03) VALUE 'REH'.
           05  FILLER PIC X(03) VALUE 'LAB'.
           05  FILLER PIC X(03) VALUE 'PRV'.
           05  FILLER PIC X(03) VALUE 'PED'.

      *----------------------------------------------------------------*
      * PCP ASSIGNMENT WORK AREA
      *----------------------------------------------------------------*
       01  WS-PCP-WORK.
           05  WS-PCP-NPI             PIC X(10).
           05  WS-PCP-NAME            PIC X(50).
           05  WS-PCP-NETWORK-CD      PIC X(06).
           05  WS-PCP-PANEL-LIMIT     PIC 9(05).
           05  WS-PCP-PANEL-CURRENT   PIC 9(05).
           05  WS-PCP-ACCEPTING       PIC X(01).
               88  WS-PCP-IS-ACCEPTING VALUE 'Y'.
           05  WS-PCP-SPECIALTY       PIC X(03).
           05  WS-PCP-ZIP             PIC X(05).
           05  WS-PCP-DISTANCE        PIC 9(04).

      *----------------------------------------------------------------*
      * COB (COORDINATION OF BENEFITS) WORK AREA
      *----------------------------------------------------------------*
       01  WS-COB-WORK.
           05  WS-COB-HAS-OTHER-INS   PIC X(01) VALUE 'N'.
               88  WS-HAS-OTHER-INS   VALUE 'Y'.
           05  WS-COB-OTHER-PAYER     PIC X(08).
           05  WS-COB-OTHER-MEMBER-ID PIC X(20).
           05  WS-COB-OTHER-GROUP     PIC X(15).
           05  WS-COB-SEQ             PIC 9(01).
               88  WS-COB-IS-PRIMARY  VALUE 1.
               88  WS-COB-IS-SECONDARY VALUE 2.
               88  WS-COB-IS-TERTIARY VALUE 3.
           05  WS-COB-MCARE-FLAG      PIC X(01) VALUE 'N'.
               88  WS-HAS-MEDICARE    VALUE 'Y'.
           05  WS-COB-MCARE-PART-A    PIC X(01).
           05  WS-COB-MCARE-PART-B    PIC X(01).
           05  WS-COB-MCARE-PART-D    PIC X(01).
           05  WS-COB-MCAID-FLAG      PIC X(01) VALUE 'N'.
               88  WS-HAS-MEDICAID    VALUE 'Y'.
           05  WS-COB-DUAL-ELIG       PIC X(01) VALUE 'N'.
               88  WS-IS-DUAL-ELIGIBLE VALUE 'Y'.
           05  WS-COB-MSP-CODE        PIC X(02).
               88  WS-MSP-WORKING-AGED VALUE '12'.
               88  WS-MSP-ESRD        VALUE '41'.
               88  WS-MSP-DISABILITY  VALUE '43'.

      *----------------------------------------------------------------*
      * SPECIAL PROGRAMS TABLE
      *----------------------------------------------------------------*
       01  WS-SPECIAL-PGMS.
           05  WS-SP-DM-DIABETES      PIC X(01) VALUE 'N'.
               88  WS-IN-DM-DIABETES  VALUE 'Y'.
           05  WS-SP-DM-CHF           PIC X(01) VALUE 'N'.
               88  WS-IN-DM-CHF       VALUE 'Y'.
           05  WS-SP-DM-COPD          PIC X(01) VALUE 'N'.
               88  WS-IN-DM-COPD      VALUE 'Y'.
           05  WS-SP-DM-ASTHMA        PIC X(01) VALUE 'N'.
               88  WS-IN-DM-ASTHMA    VALUE 'Y'.
           05  WS-SP-CASE-MGMT        PIC X(01) VALUE 'N'.
               88  WS-IN-CASE-MGMT    VALUE 'Y'.
           05  WS-SP-MATERNITY        PIC X(01) VALUE 'N'.
               88  WS-IN-MATERNITY    VALUE 'Y'.
           05  WS-SP-TRANSPLANT       PIC X(01) VALUE 'N'.
               88  WS-IN-TRANSPLANT   VALUE 'Y'.
           05  WS-SP-WELLNESS         PIC X(01) VALUE 'N'.
               88  WS-IN-WELLNESS     VALUE 'Y'.
           05  WS-SP-CHRONIC-CARE     PIC X(01) VALUE 'N'.
               88  WS-IN-CHRONIC-CARE VALUE 'Y'.

      *----------------------------------------------------------------*
      * COBRA NOTIFICATION LETTER WORK AREA
      *----------------------------------------------------------------*
       01  WS-COBRA-LETTER.
           05  WS-CL-MEMBER-ID        PIC X(12).
           05  WS-CL-MEMBER-NAME      PIC X(50).
           05  WS-CL-QUAL-EVENT-CD    PIC X(02).
           05  WS-CL-QUAL-EVENT-DESC  PIC X(50).
           05  WS-CL-EVENT-DATE       PIC X(08).
           05  WS-CL-NOTICE-DATE      PIC X(08).
           05  WS-CL-ELECT-DEADLINE   PIC X(08).
           05  WS-CL-COVERAGE-END     PIC X(08).
           05  WS-CL-PREMIUM-AMT      PIC S9(07)V99 COMP-3.
           05  WS-CL-PREMIUM-DISABLED PIC S9(07)V99 COMP-3.
           05  WS-CL-PLAN-CODE        PIC X(08).
           05  WS-CL-PLAN-NAME        PIC X(40).
           05  WS-CL-SUBSIDY-IND      PIC X(01).
           05  FILLER                 PIC X(155).

      *----------------------------------------------------------------*
      * RECOUPMENT WORK AREA
      *----------------------------------------------------------------*
       01  WS-RECOUP-WORK.
           05  WS-RC-CLAIM-COUNT      PIC 9(05) VALUE ZEROES.
           05  WS-RC-TOTAL-AMOUNT     PIC S9(09)V99 COMP-3
                                                  VALUE ZEROES.

      *----------------------------------------------------------------*
      * DATABASE RETRY CONTROL
      *----------------------------------------------------------------*
       01  WS-DB-RETRY.
           05  WS-DB-RETRY-COUNT      PIC 9(02) VALUE ZEROES.
           05  WS-DB-MAX-RETRIES      PIC 9(02) VALUE 03.
           05  WS-DB-SQLCODE          PIC S9(09) COMP VALUE ZEROES.
           05  WS-DB-OPERATION        PIC X(30) VALUE SPACES.

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
               88  WS-271-PENDING             VALUE 'P'.
               88  WS-271-RUNOUT              VALUE 'R'.
               88  WS-271-GRACE               VALUE 'G'.
           05  WS-271-PLAN-CODE        PIC X(08).
           05  WS-271-PLAN-NAME        PIC X(40).
           05  WS-271-PLAN-TYPE        PIC X(03).
           05  WS-271-EFF-DATE         PIC X(08).
           05  WS-271-TERM-DATE        PIC X(08).
           05  WS-271-DEDUCT-IND-IN    PIC S9(07)V99.
           05  WS-271-DEDUCT-REM-IN    PIC S9(07)V99.
           05  WS-271-DEDUCT-IND-OON   PIC S9(07)V99.
           05  WS-271-DEDUCT-REM-OON   PIC S9(07)V99.
           05  WS-271-DEDUCT-FAM-IN    PIC S9(07)V99.
           05  WS-271-DEDUCT-FAM-REM   PIC S9(07)V99.
           05  WS-271-OOP-IND-IN       PIC S9(07)V99.
           05  WS-271-OOP-REM-IN       PIC S9(07)V99.
           05  WS-271-OOP-FAM-IN       PIC S9(07)V99.
           05  WS-271-OOP-FAM-REM      PIC S9(07)V99.
           05  WS-271-COPAY-PCP        PIC S9(05)V99.
           05  WS-271-COPAY-SPEC       PIC S9(05)V99.
           05  WS-271-COPAY-ER         PIC S9(05)V99.
           05  WS-271-COPAY-MH         PIC S9(05)V99.
           05  WS-271-COINSURANCE-IN   PIC 9(03).
           05  WS-271-COINSURANCE-OON  PIC 9(03).
           05  WS-271-PCP-NPI          PIC X(10).
           05  WS-271-PCP-NAME         PIC X(50).
           05  WS-271-PCP-REQUIRED     PIC X(01).
           05  WS-271-REFERRAL-REQD    PIC X(01).
           05  WS-271-COB-IND          PIC X(01).
           05  WS-271-COB-PAYER        PIC X(08).
           05  WS-271-COB-SEQ          PIC 9(01).
           05  WS-271-MCARE-IND        PIC X(01).
           05  WS-271-MCAID-IND        PIC X(01).
           05  WS-271-NETWORK-CD       PIC X(06).
           05  WS-271-PT-REMAIN        PIC 9(03).
           05  WS-271-OT-REMAIN        PIC 9(03).
           05  WS-271-MH-REMAIN        PIC 9(03).
           05  WS-271-CHIRO-REMAIN     PIC 9(03).
           05  WS-271-SPEECH-REMAIN    PIC 9(03).
           05  WS-271-SUD-REMAIN       PIC 9(03).
           05  WS-271-LIFETIME-REMAIN  PIC S9(09)V99.
           05  WS-271-ANNUAL-REMAIN    PIC S9(09)V99.
           05  WS-271-GRANDFATHERED    PIC X(01).
           05  WS-271-SPECIAL-PGMS     PIC X(09).
           05  WS-271-REJECT-REASON    PIC X(03).
           05  WS-271-REJECT-MSG       PIC X(50).
           05  WS-271-WAITING-PERIOD   PIC X(01).
           05  WS-271-PREEXIST-FLAG    PIC X(01).
           05  FILLER                  PIC X(81).

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
           05  WS-AUD-TRANS-TYPE       PIC X(03).
           05  WS-AUD-PLAN-CODE        PIC X(08).
           05  FILLER                  PIC X(130).

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
      * ERROR HANDLING WORK AREA
      *----------------------------------------------------------------*
       01  WS-ERROR-WORK.
           05  WS-ERR-CODE            PIC X(06) VALUE SPACES.
           05  WS-ERR-MSG             PIC X(80) VALUE SPACES.
           05  WS-ERR-SEV             PIC X(01) VALUE SPACES.
               88  WS-ERR-IS-INFO    VALUE 'I'.
               88  WS-ERR-IS-WARN    VALUE 'W'.
               88  WS-ERR-IS-ERROR   VALUE 'E'.
               88  WS-ERR-IS-FATAL   VALUE 'F'.
           05  WS-ERR-COUNT           PIC 9(06) VALUE ZEROES.
           05  WS-ERR-MAX             PIC 9(06) VALUE 999.

      *----------------------------------------------------------------*
      * WORK FIELDS
      *----------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-SUB-1               PIC 9(03).
           05  WS-SUB-2               PIC 9(03).
           05  WS-PLAN-IDX            PIC 9(03).
           05  WS-MEMBER-ID-WORK      PIC X(12).
           05  WS-SUBSCRIBER-ID-WORK  PIC X(12).
           05  WS-PLAN-CODE-WORK      PIC X(08).
           05  WS-WORK-AMOUNT         PIC S9(09)V99 COMP-3.
           05  WS-MEMBER-COUNT-WORK   PIC 9(05).
           05  WS-DED-MET-COUNT       PIC 9(02).
           05  WS-EMBEDDED-THRESHOLD  PIC 9(02).
           05  WS-PREMIUM-FULL        PIC S9(07)V99 COMP-3.
           05  WS-PREMIUM-COBRA       PIC S9(07)V99 COMP-3.
           05  WS-NEW-MEMBER-ID       PIC X(12).
           05  WS-SEQUENCE-NO         PIC 9(08).
           05  WS-NETWORK-MATCH       PIC X(01).
           05  WS-RETURN-CODE         PIC S9(04) COMP VALUE 0.

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
           05  HV-ME-PCP-NPI          PIC X(10).
           05  HV-ME-NETWORK-CD       PIC X(06).
           05  HV-ME-COVERAGE-LEVEL   PIC X(02).
           05  HV-ME-DIVISION-CD      PIC X(04).
           05  HV-ME-CLASS-CD         PIC X(04).
           05  HV-ME-DISABILITY-IND   PIC X(01).
           05  HV-ME-DISABILITY-DT    PIC X(08).
           05  HV-ME-GRANDFATHERED    PIC X(01).
           05  HV-ME-PLAN-TYPE        PIC X(03).

       01  HV-MEMBER-ACCUM.
           05  HV-MA-MEMBER-ID        PIC X(12).
           05  HV-MA-PLAN-YEAR        PIC X(04).
           05  HV-MA-DED-USED-IN      PIC S9(07)V99.
           05  HV-MA-DED-USED-OON     PIC S9(07)V99.
           05  HV-MA-OOP-USED-IN      PIC S9(07)V99.
           05  HV-MA-OOP-USED-OON     PIC S9(07)V99.
           05  HV-MA-LIFETIME-USED    PIC S9(09)V99.
           05  HV-MA-ANNUAL-USED      PIC S9(09)V99.
           05  HV-MA-CARRYOVER        PIC S9(07)V99.
           05  HV-MA-LAST-UPDATED     PIC X(26).

       01  HV-FAMILY-ACCUM.
           05  HV-FA-SUBSCRIBER-ID    PIC X(12).
           05  HV-FA-PLAN-YEAR        PIC X(04).
           05  HV-FA-DED-USED-IN      PIC S9(07)V99.
           05  HV-FA-DED-USED-OON     PIC S9(07)V99.
           05  HV-FA-OOP-USED-IN      PIC S9(07)V99.
           05  HV-FA-OOP-USED-OON     PIC S9(07)V99.
           05  HV-FA-LAST-UPDATED     PIC X(26).

       01  HV-PLAN-BENEFIT.
           05  HV-PB-PLAN-CODE        PIC X(08).
           05  HV-PB-PLAN-TYPE        PIC X(03).
           05  HV-PB-IND-DED-IN       PIC S9(07)V99.
           05  HV-PB-IND-DED-OON      PIC S9(07)V99.
           05  HV-PB-FAM-DED-IN       PIC S9(07)V99.
           05  HV-PB-FAM-DED-OON      PIC S9(07)V99.
           05  HV-PB-EMBEDDED-FL      PIC X(01).
           05  HV-PB-EMBED-CNT        PIC 9(01).
           05  HV-PB-IND-OOP-IN       PIC S9(07)V99.
           05  HV-PB-IND-OOP-OON      PIC S9(07)V99.
           05  HV-PB-FAM-OOP-IN       PIC S9(07)V99.
           05  HV-PB-FAM-OOP-OON      PIC S9(07)V99.
           05  HV-PB-LIFETIME-MAX     PIC S9(09)V99.
           05  HV-PB-ANNUAL-MAX       PIC S9(09)V99.
           05  HV-PB-COPAY-PCP        PIC S9(05)V99.
           05  HV-PB-COPAY-SPEC       PIC S9(05)V99.
           05  HV-PB-COPAY-ER         PIC S9(05)V99.
           05  HV-PB-COPAY-MH         PIC S9(05)V99.
           05  HV-PB-COINSURANCE-IN   PIC 9(03).
           05  HV-PB-COINSURANCE-OON  PIC 9(03).
           05  HV-PB-PCP-REQD         PIC X(01).
           05  HV-PB-REFERRAL-RQ      PIC X(01).
           05  HV-PB-GRANDFATHERED    PIC X(01).
           05  HV-PB-WAIT-DAYS        PIC 9(03).
           05  HV-PB-WAIT-TYPE        PIC X(01).
           05  HV-PB-PREEXIST-FL      PIC X(01).
           05  HV-PB-PLAN-YEAR-START  PIC X(04).
           05  HV-PB-EHB-COMPLIANT    PIC X(01).
           05  HV-PB-METAL-TIER       PIC X(02).
           05  HV-PB-AV-PERCENT       PIC 9(03).
           05  HV-PB-NETWORK-CD       PIC X(06).
           05  HV-PB-PT-VISITS        PIC 9(03).
           05  HV-PB-OT-VISITS        PIC 9(03).
           05  HV-PB-MH-VISITS        PIC 9(03).
           05  HV-PB-CHIRO-VISITS     PIC 9(03).
           05  HV-PB-SPEECH-VISITS    PIC 9(03).
           05  HV-PB-SUD-VISITS       PIC 9(03).
           05  HV-PB-PLAN-NAME        PIC X(40).

       01  HV-VISIT-LIMITS.
           05  HV-VL-MEMBER-ID        PIC X(12).
           05  HV-VL-PLAN-YEAR        PIC X(04).
           05  HV-VL-PT-USED          PIC 9(03).
           05  HV-VL-OT-USED          PIC 9(03).
           05  HV-VL-MH-USED          PIC 9(03).
           05  HV-VL-CHIRO-USED       PIC 9(03).
           05  HV-VL-SPEECH-USED      PIC 9(03).
           05  HV-VL-SUD-USED         PIC 9(03).

       01  HV-PCP-INFO.
           05  HV-PCP-NPI             PIC X(10).
           05  HV-PCP-NAME            PIC X(50).
           05  HV-PCP-NETWORK-CD      PIC X(06).
           05  HV-PCP-PANEL-LIMIT     PIC 9(05).
           05  HV-PCP-PANEL-CURRENT   PIC 9(05).
           05  HV-PCP-ACCEPTING       PIC X(01).
           05  HV-PCP-SPECIALTY       PIC X(03).
           05  HV-PCP-ZIP             PIC X(05).
           05  HV-PCP-STATUS          PIC X(01).

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
           05  HV-CT-PREMIUM-AMT      PIC S9(07)V99.

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

       01  HV-COB-INFO.
           05  HV-COB-MEMBER-ID       PIC X(12).
           05  HV-COB-OTHER-PAYER     PIC X(08).
           05  HV-COB-OTHER-MBR-ID    PIC X(20).
           05  HV-COB-OTHER-GROUP     PIC X(15).
           05  HV-COB-SEQ             PIC 9(01).
           05  HV-COB-EFF-DATE        PIC X(08).
           05  HV-COB-TERM-DATE       PIC X(08).
           05  HV-COB-MCARE-FLAG      PIC X(01).
           05  HV-COB-MCARE-PART-A    PIC X(01).
           05  HV-COB-MCARE-PART-B    PIC X(01).
           05  HV-COB-MCARE-PART-D    PIC X(01).
           05  HV-COB-MCAID-FLAG      PIC X(01).
           05  HV-COB-DUAL-ELIG       PIC X(01).
           05  HV-COB-MSP-CODE        PIC X(02).

       01  HV-SPECIAL-PGMS.
           05  HV-SPG-MEMBER-ID       PIC X(12).
           05  HV-SPG-PROGRAM-CD      PIC X(04).
           05  HV-SPG-ENROLL-DT       PIC X(08).
           05  HV-SPG-STATUS          PIC X(01).

       01  HV-SERVICE-DATE            PIC X(08).
       01  HV-PLAN-CODE-IN            PIC X(08).
       01  HV-SUBSCRIBER-ID-IN        PIC X(12).
       01  HV-CLAIM-COUNT             PIC S9(09) COMP.
       01  HV-RECOUP-TOTAL            PIC S9(09)V99.
       01  HV-SEQUENCE-NO             PIC 9(08).
       01  HV-FAMILY-MBR-COUNT        PIC 9(03).

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
           STRING WS-CURR-YYYY WS-CURR-MM WS-CURR-DD
               DELIMITED BY SIZE INTO WS-CURR-DATE-8

           PERFORM 1100-CONNECT-DATABASE
           PERFORM 1200-OPEN-FILES
           PERFORM 1300-LOAD-PLAN-TABLE

           INITIALIZE WS-COUNTERS
           INITIALIZE WS-FLAGS
           MOVE 'N' TO WS-834-EOF-FLAG
           MOVE 'N' TO WS-270-EOF-FLAG

           DISPLAY 'HCELIGVR: INITIALIZATION COMPLETE'
           DISPLAY 'HCELIGVR: PROCESSING DATE: ' WS-CURR-DATE-8
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

           DISPLAY 'HCELIGVR: DATABASE CONNECTION ESTABLISHED'
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
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ELIG-271-FILE'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT ENRL-RPT-FILE
           IF WS-ENRL-RPT-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ENRL-RPT-FILE'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT COBRA-LTR-FILE
           IF WS-COBRA-LTR-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN COBRA-LTR-FILE'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT ACA-RECON-OUT
           IF WS-ACA-RECON-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ACA-RECON-OUT'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT ACCUM-RPT-FILE
           IF WS-ACCUM-RPT-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ACCUM-RPT-FILE'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT RECOUP-FILE
           IF WS-RECOUP-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN RECOUP-FILE'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT ERROR-FILE
           IF WS-ERROR-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN ERROR-FILE'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           OPEN OUTPUT AUDIT-TRAIL-FILE
           IF WS-AUDIT-STATUS NOT = '00'
               DISPLAY 'HCELIGVR: UNABLE TO OPEN AUDIT-TRAIL-FILE'
               MOVE 811 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF
           .

      *----------------------------------------------------------------*
       1300-LOAD-PLAN-TABLE.
      *----------------------------------------------------------------*
      *    LOAD ALL ACTIVE PLAN CONFIGURATIONS INTO MEMORY TABLE
           MOVE ZEROES TO WS-PLAN-TABLE-COUNT

           EXEC SQL
               DECLARE PLAN_CURSOR CURSOR FOR
               SELECT plan_code, plan_type,
                      ind_ded_in, ind_ded_oon,
                      fam_ded_in, fam_ded_oon,
                      embedded_fl, embed_cnt,
                      ind_oop_in, ind_oop_oon,
                      fam_oop_in, fam_oop_oon,
                      lifetime_max, annual_max,
                      copay_pcp, copay_spec, copay_er,
                      copay_urgent, copay_ip,
                      copay_rx_generic, copay_rx_brand,
                      copay_rx_specialty, copay_mh, copay_pt,
                      coinsur_in, coinsur_oon,
                      pcp_required, referral_required,
                      grandfathered, wait_period_days,
                      wait_type, preexist_flag,
                      plan_year_start, ehb_compliant,
                      metal_tier, av_percent, network_cd,
                      pt_visits, ot_visits, mh_visits,
                      chiro_visits, speech_visits, sud_visits
               FROM   T_PLAN_BENEFIT
               WHERE  plan_status = 'A'
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCELIGVR: PLAN CURSOR DECLARE FAILED'
               MOVE 850 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           EXEC SQL OPEN PLAN_CURSOR END-EXEC

           PERFORM UNTIL SQLCODE NOT = 0
               ADD 1 TO WS-PLAN-TABLE-COUNT
               IF WS-PLAN-TABLE-COUNT > 50
                   DISPLAY 'HCELIGVR: PLAN TABLE OVERFLOW > 50'
                   MOVE 850 TO ERR-ABEND-CODE
                   PERFORM 9500-ABEND-ROUTINE
               END-IF

               EXEC SQL
                   FETCH PLAN_CURSOR
                   INTO :HV-PB-PLAN-CODE, :HV-PB-PLAN-TYPE,
                        :HV-PB-IND-DED-IN, :HV-PB-IND-DED-OON,
                        :HV-PB-FAM-DED-IN, :HV-PB-FAM-DED-OON,
                        :HV-PB-EMBEDDED-FL, :HV-PB-EMBED-CNT,
                        :HV-PB-IND-OOP-IN, :HV-PB-IND-OOP-OON,
                        :HV-PB-FAM-OOP-IN, :HV-PB-FAM-OOP-OON,
                        :HV-PB-LIFETIME-MAX, :HV-PB-ANNUAL-MAX,
                        :HV-PB-COPAY-PCP, :HV-PB-COPAY-SPEC,
                        :HV-PB-COPAY-ER,
                        :WS-PLN-COPAY-URGENT(WS-PLAN-TABLE-COUNT),
                        :WS-PLN-COPAY-IP(WS-PLAN-TABLE-COUNT),
                        :WS-PLN-COPAY-RX-GN(WS-PLAN-TABLE-COUNT),
                        :WS-PLN-COPAY-RX-BR(WS-PLAN-TABLE-COUNT),
                        :WS-PLN-COPAY-RX-SP(WS-PLAN-TABLE-COUNT),
                        :HV-PB-COPAY-MH,
                        :WS-PLN-COPAY-PT(WS-PLAN-TABLE-COUNT),
                        :HV-PB-COINSURANCE-IN,
                        :HV-PB-COINSURANCE-OON,
                        :HV-PB-PCP-REQD, :HV-PB-REFERRAL-RQ,
                        :HV-PB-GRANDFATHERED, :HV-PB-WAIT-DAYS,
                        :HV-PB-WAIT-TYPE, :HV-PB-PREEXIST-FL,
                        :HV-PB-PLAN-YEAR-START,
                        :HV-PB-EHB-COMPLIANT,
                        :HV-PB-METAL-TIER, :HV-PB-AV-PERCENT,
                        :HV-PB-NETWORK-CD,
                        :HV-PB-PT-VISITS, :HV-PB-OT-VISITS,
                        :HV-PB-MH-VISITS, :HV-PB-CHIRO-VISITS,
                        :HV-PB-SPEECH-VISITS, :HV-PB-SUD-VISITS
               END-EXEC

               IF SQLCODE = 0
                   MOVE HV-PB-PLAN-CODE TO
                       WS-PLN-PLAN-CODE(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-PLAN-TYPE TO
                       WS-PLN-PLAN-TYPE(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-IND-DED-IN TO
                       WS-PLN-IND-DED-IN(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-IND-DED-OON TO
                       WS-PLN-IND-DED-OON(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-FAM-DED-IN TO
                       WS-PLN-FAM-DED-IN(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-FAM-DED-OON TO
                       WS-PLN-FAM-DED-OON(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-EMBEDDED-FL TO
                       WS-PLN-EMBEDDED-FL(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-EMBED-CNT TO
                       WS-PLN-EMBED-CNT(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-IND-OOP-IN TO
                       WS-PLN-IND-OOP-IN(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-IND-OOP-OON TO
                       WS-PLN-IND-OOP-OON(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-FAM-OOP-IN TO
                       WS-PLN-FAM-OOP-IN(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-FAM-OOP-OON TO
                       WS-PLN-FAM-OOP-OON(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-LIFETIME-MAX TO
                       WS-PLN-LIFETIME-MX(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-ANNUAL-MAX TO
                       WS-PLN-ANNUAL-MAX(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-COPAY-PCP TO
                       WS-PLN-COPAY-PCP(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-COPAY-SPEC TO
                       WS-PLN-COPAY-SPEC(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-COPAY-ER TO
                       WS-PLN-COPAY-ER(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-COPAY-MH TO
                       WS-PLN-COPAY-MH(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-COINSURANCE-IN TO
                       WS-PLN-COINSUR-IN(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-COINSURANCE-OON TO
                       WS-PLN-COINSUR-OON(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-PCP-REQD TO
                       WS-PLN-PCP-REQD(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-REFERRAL-RQ TO
                       WS-PLN-REFERRAL-RQ(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-GRANDFATHERED TO
                       WS-PLN-GRANDFATHRD(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-WAIT-DAYS TO
                       WS-PLN-WAIT-PERIOD(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-WAIT-TYPE TO
                       WS-PLN-WAIT-TYPE(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-PREEXIST-FL TO
                       WS-PLN-PREEXIST-FL(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-PLAN-YEAR-START TO
                       WS-PLN-YEAR-START(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-EHB-COMPLIANT TO
                       WS-PLN-EHB-COMPL(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-METAL-TIER TO
                       WS-PLN-METAL-TIER(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-AV-PERCENT TO
                       WS-PLN-AV-PCT(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-NETWORK-CD TO
                       WS-PLN-NETWORK-CD(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-PT-VISITS TO
                       WS-PLN-PT-VISITS(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-OT-VISITS TO
                       WS-PLN-OT-VISITS(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-MH-VISITS TO
                       WS-PLN-MH-VISITS(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-CHIRO-VISITS TO
                       WS-PLN-CHIRO-VISIT(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-SPEECH-VISITS TO
                       WS-PLN-SPEECH-VIS(WS-PLAN-TABLE-COUNT)
                   MOVE HV-PB-SUD-VISITS TO
                       WS-PLN-SUD-VISITS(WS-PLAN-TABLE-COUNT)
               END-IF
           END-PERFORM

           EXEC SQL CLOSE PLAN_CURSOR END-EXEC

           SUBTRACT 1 FROM WS-PLAN-TABLE-COUNT
           DISPLAY 'HCELIGVR: LOADED ' WS-PLAN-TABLE-COUNT
               ' PLAN CONFIGURATIONS'
           .

      *================================================================*
      * 834 ENROLLMENT TRANSACTION PROCESSING
      *================================================================*

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
                   WHEN E834-REACTIVATION
                       PERFORM 2810-REACTIVATION
                   WHEN E834-ADDRESS-CHANGE
                       PERFORM 2820-ADDRESS-CHANGE
                   WHEN E834-PCP-CHANGE
                       PERFORM 2830-PCP-CHANGE
                   WHEN E834-NEWBORN-ADD
                       PERFORM 2840-NEWBORN-ENROLLMENT
                   WHEN E834-QMCSO-ORDER
                       PERFORM 2850-QMCSO-PROCESSING
                   WHEN E834-DISABILITY-EXT
                       PERFORM 2860-DISABILITY-EXTENSION
                   WHEN OTHER
                       ADD 1 TO WS-834-ERROR-CNT
                       MOVE 'EL0001' TO WS-ERR-CODE
                       STRING 'INVALID TRANS TYPE: '
                           E834-TRANS-TYPE
                           DELIMITED BY SIZE INTO WS-ERR-MSG
                       MOVE 'E' TO WS-ERR-SEV
                       PERFORM 8200-WRITE-ERROR-RECORD
                       MOVE 'INVALID TRANS TYPE' TO WS-RPT-MSG
                       PERFORM 8100-WRITE-DETAIL-LINE
               END-EVALUATE

               EXEC SQL COMMIT WORK END-EXEC

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
      *    STEP 1: VALIDATE DEMOGRAPHICS
           IF E834-LAST-NAME = SPACES OR LOW-VALUES
               MOVE 'EL0010' TO WS-ERR-CODE
               MOVE 'MEMBER LAST NAME IS REQUIRED' TO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               MOVE 'MISSING LAST NAME  ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2100-EXIT
           END-IF

           IF E834-DOB = SPACES OR E834-DOB = ZEROES
               MOVE 'EL0011' TO WS-ERR-CODE
               MOVE 'MEMBER DOB IS REQUIRED' TO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               MOVE 'MISSING DOB        ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2100-EXIT
           END-IF

           IF E834-DOB > WS-CURR-DATE-8
               MOVE 'EL0012' TO WS-ERR-CODE
               MOVE 'MEMBER DOB CANNOT BE IN FUTURE' TO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               GO TO 2100-EXIT
           END-IF

           IF NOT (E834-MALE OR E834-FEMALE OR E834-UNKNOWN-GENDER)
               MOVE 'EL0013' TO WS-ERR-CODE
               MOVE 'INVALID GENDER CODE' TO WS-ERR-MSG
               MOVE 'W' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
           END-IF

      *    STEP 2: SSN DUPLICATE CHECK
           IF E834-MEMBER-SSN NOT = SPACES AND
              E834-MEMBER-SSN NOT = '000000000'
               MOVE 'N' TO WS-SSN-DUP-FLAG

               EXEC SQL
                   SELECT COUNT(*)
                   INTO   :WS-MEMBER-COUNT-WORK
                   FROM   T_MEMBER_ELIG
                   WHERE  ssn = :E834-MEMBER-SSN
                   AND    status = 'A'
                   AND    member_id <> :E834-MEMBER-ID
               END-EXEC

               IF SQLCODE = 0 AND WS-MEMBER-COUNT-WORK > 0
                   SET WS-SSN-IS-DUPLICATE TO TRUE
                   MOVE 'EL0014' TO WS-ERR-CODE
                   STRING 'DUPLICATE SSN FOUND - '
                       WS-MEMBER-COUNT-WORK ' ACTIVE RECORDS'
                       DELIMITED BY SIZE INTO WS-ERR-MSG
                   MOVE 'W' TO WS-ERR-SEV
                   PERFORM 8200-WRITE-ERROR-RECORD
               END-IF
           END-IF

      *    STEP 3: VERIFY MEMBER NOT ALREADY ACTIVE
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
               MOVE 'EL0015' TO WS-ERR-CODE
               MOVE 'MEMBER ALREADY HAS ACTIVE ENROLLMENT'
                   TO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               MOVE 'ALREADY ACTIVE     ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2100-EXIT
           END-IF

      *    STEP 4: VALIDATE PLAN CODE
           MOVE E834-PLAN-CODE TO HV-PLAN-CODE-IN
           PERFORM 4000-VALIDATE-PLAN

           IF WS-PLAN-INVALID
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'INVALID PLAN CODE  ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2100-EXIT
           END-IF

      *    STEP 5: PCP ASSIGNMENT FOR HMO/POS
           IF WS-PLN-PCP-REQD(WS-PLAN-IDX) = 'Y'
               SET WS-PCP-REQUIRED TO TRUE
               IF E834-PCP-NPI NOT = SPACES
                   PERFORM 4050-VALIDATE-PCP
                   IF WS-PCP-INVALID
                       MOVE 'EL0020' TO WS-ERR-CODE
                       MOVE 'PCP NPI INVALID - AUTO-ASSIGN'
                           TO WS-ERR-MSG
                       MOVE 'W' TO WS-ERR-SEV
                       PERFORM 8200-WRITE-ERROR-RECORD
                       PERFORM 4060-AUTO-ASSIGN-PCP
                   END-IF
               ELSE
                   PERFORM 4060-AUTO-ASSIGN-PCP
               END-IF
           END-IF

      *    STEP 6: DEPENDENT ELIGIBILITY CHECK
           IF E834-IS-DEPENDENT
               PERFORM 4100-CALCULATE-AGE
               IF WS-AGE-YEARS >= WS-DEPEND-AGE-LIMIT
                   IF NOT E834-IS-DISABLED
                       MOVE 'EL0025' TO WS-ERR-CODE
                       MOVE 'DEPENDENT EXCEEDS AGE LIMIT'
                           TO WS-ERR-MSG
                       MOVE 'E' TO WS-ERR-SEV
                       PERFORM 8200-WRITE-ERROR-RECORD
                       MOVE 'DEP OVER AGE LIMIT ' TO WS-RPT-MSG
                       PERFORM 8100-WRITE-DETAIL-LINE
                       GO TO 2100-EXIT
                   END-IF
               END-IF
           END-IF

      *    STEP 7: WAITING PERIOD CALCULATION
           IF WS-PLN-WAIT-PERIOD(WS-PLAN-IDX) > 0
               PERFORM 4070-CALCULATE-WAITING-PERIOD
               IF WS-IN-WAITING-PERIOD
                   MOVE 'EL0028' TO WS-ERR-CODE
                   STRING 'WAITING PERIOD: EFF DATE ADJUSTED TO '
                       WS-FIRST-OF-MONTH-AFTER
                       DELIMITED BY SIZE INTO WS-ERR-MSG
                   MOVE 'I' TO WS-ERR-SEV
                   PERFORM 8200-WRITE-ERROR-RECORD
      *            OVERRIDE EFF DATE TO FIRST OF MONTH AFTER WAIT
                   MOVE WS-FIRST-OF-MONTH-AFTER TO E834-EFF-DATE
               END-IF
           END-IF

      *    STEP 8: ACA EXCHANGE PROCESSING
           IF E834-IS-EXCHANGE
               PERFORM 5000-ACA-EXCHANGE-PROCESSING
               IF WS-ACA-INVALID
                   GO TO 2100-EXIT
               END-IF
           END-IF

      *    STEP 9: PRE-EXISTING CONDITION CHECK (GRANDFATHERED ONLY)
           IF WS-PLN-GRANDFATHRD(WS-PLAN-IDX) = 'Y'
               AND WS-PLN-PREEXIST-FL(WS-PLAN-IDX) = 'Y'
               PERFORM 4080-CHECK-PREEXISTING
           END-IF

      *    STEP 10: INSERT ELIGIBILITY RECORD
           EXEC SQL
               INSERT INTO T_MEMBER_ELIG
                   (member_id, ssn, last_name,
                    first_name, dob, gender,
                    plan_code, group_num,
                    division_cd, class_cd,
                    coverage_level,
                    eff_date, term_date, status,
                    subscriber_id, relation_code,
                    cobra_ind, aca_exchange_ind,
                    metal_tier, pcp_npi, network_cd,
                    disability_ind, disability_dt,
                    grandfathered, plan_type,
                    create_dt, create_pgm)
               VALUES
                   (:E834-MEMBER-ID,
                    :E834-MEMBER-SSN,
                    :E834-LAST-NAME,
                    :E834-FIRST-NAME,
                    :E834-DOB,
                    :E834-GENDER,
                    :E834-PLAN-CODE,
                    :E834-GROUP-NUMBER,
                    :E834-DIVISION-CODE,
                    :E834-CLASS-CODE,
                    :E834-COVERAGE-LEVEL,
                    :E834-EFF-DATE,
                    '99991231', 'A',
                    :E834-MEMBER-ID,
                    :E834-RELATION-CODE,
                    'N',
                    :E834-ACA-EXCHANGE-IND,
                    :E834-METAL-TIER,
                    :WS-PCP-NPI,
                    :WS-PLN-NETWORK-CD(WS-PLAN-IDX),
                    :E834-DISABILITY-IND,
                    :E834-DISABILITY-DT,
                    :WS-PLN-GRANDFATHRD(WS-PLAN-IDX),
                    :WS-PLN-PLAN-TYPE(WS-PLAN-IDX),
                    :WS-CURR-DATE-8,
                    'HCELIGVR')
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-NEW-ENROLL-CNT

      *        INITIALIZE BENEFIT ACCUMULATORS
               PERFORM 4200-INITIALIZE-ACCUMULATORS

      *        INITIALIZE VISIT LIMITS
               PERFORM 4210-INITIALIZE-VISIT-LIMITS

      *        CHECK FOR RETROACTIVE ENROLLMENT
               IF E834-EFF-DATE < WS-CURR-DATE-8
                   SET WS-IS-RETRO TO TRUE
                   MOVE 'EL0030' TO WS-ERR-CODE
                   STRING 'RETROACTIVE ENROLLMENT FROM '
                       E834-EFF-DATE ' - CLAIMS FLAGGED'
                       DELIMITED BY SIZE INTO WS-ERR-MSG
                   MOVE 'I' TO WS-ERR-SEV
                   PERFORM 8200-WRITE-ERROR-RECORD
                   PERFORM 6100-FLAG-CLAIMS-REPROCESS
               END-IF

      *        AUDIT TRAIL
               MOVE 'ENRL' TO WS-AUD-ACTION
               MOVE E834-PLAN-CODE TO WS-AUD-NEW-VALUE
               PERFORM 7000-WRITE-AUDIT-TRAIL

               MOVE 'ENROLLED OK        ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'EL0035' TO WS-ERR-CODE
               STRING 'DB INSERT FAILED SQLCODE='
                   SQLCODE DELIMITED BY SIZE INTO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               MOVE 'DB INSERT FAILED   ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2100-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2200-MEMBER-TERMINATION.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

      *    VALIDATE TERM REASON CODE
           IF E834-TERM-REASON-CODE = SPACES
               MOVE '099' TO E834-TERM-REASON-CODE
           END-IF

      *    RETRIEVE CURRENT ENROLLMENT FOR AUDIT
           EXEC SQL
               SELECT plan_code, eff_date, subscriber_id,
                      relation_code, cobra_ind, aca_exchange_ind,
                      pcp_npi, coverage_level
               INTO   :HV-ME-PLAN-CODE, :HV-ME-EFF-DATE,
                      :HV-ME-SUBSCRIBER-ID,
                      :HV-ME-RELATION-CODE,
                      :HV-ME-COBRA-IND,
                      :HV-ME-ACA-EXCHANGE-IND,
                      :HV-ME-PCP-NPI,
                      :HV-ME-COVERAGE-LEVEL
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'A'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'MEMBER NOT ACTIVE  ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2200-EXIT
           END-IF

      *    APPLY TERMINATION
           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    status      = 'T',
                      term_date   = :E834-TERM-DATE,
                      term_reason = :E834-TERM-REASON-CODE,
                      update_dt   = :WS-CURR-DATE-8,
                      update_pgm  = 'HCELIGVR'
               WHERE  member_id   = :HV-ME-MEMBER-ID
               AND    status      = 'A'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-TERM-CNT
               MOVE 'TERM' TO WS-AUD-ACTION
               MOVE HV-ME-PLAN-CODE TO WS-AUD-OLD-VALUE
               MOVE E834-TERM-REASON-CODE TO WS-AUD-REASON-CODE
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'TERMINATED OK      ' TO WS-RPT-MSG

      *        COBRA ELIGIBILITY CHECK
      *        COBRA NOT AVAILABLE FOR GROSS MISCONDUCT
               IF NOT E834-TERM-GROSS-MISC
                   AND NOT E834-TERM-DEATH
                   AND NOT E834-TERM-FRAUD
                   AND NOT E834-TERM-INCARCERATED
                   AND HV-ME-COBRA-IND NOT = 'Y'
                   SET WS-COBRA-ELIGIBLE TO TRUE
                   PERFORM 2610-GENERATE-COBRA-NOTICE
               END-IF

      *        RETROACTIVE TERMINATION - RECOUPMENT
               IF E834-TERM-DATE < WS-CURR-DATE-8
                   SET WS-IS-RETRO TO TRUE
                   PERFORM 6000-RETRO-ACCUM-RECALC
                   PERFORM 6200-FLAG-CLAIMS-RECOUP
               END-IF

      *        CASCADE TERM DEPENDENTS IF SUBSCRIBER TERMED
               IF E834-REL-SELF
                   SET WS-CASCADE-TERM TO TRUE
                   PERFORM 2210-CASCADE-DEPENDENT-TERM
               END-IF

      *        DECREMENT PCP PANEL
               IF HV-ME-PCP-NPI NOT = SPACES
                   EXEC SQL
                       UPDATE T_PCP_PANEL
                       SET    panel_current = panel_current - 1
                       WHERE  pcp_npi = :HV-ME-PCP-NPI
                       AND    panel_current > 0
                   END-EXEC
               END-IF
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'TERM UPDATE FAILED ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2200-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2210-CASCADE-DEPENDENT-TERM.
      *----------------------------------------------------------------*
      *    TERMINATE ALL DEPENDENTS UNDER THIS SUBSCRIBER
           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    status      = 'T',
                      term_date   = :E834-TERM-DATE,
                      term_reason = :E834-TERM-REASON-CODE,
                      update_dt   = :WS-CURR-DATE-8,
                      update_pgm  = 'HCELIGVR'
               WHERE  subscriber_id = :HV-ME-MEMBER-ID
               AND    relation_code <> '18'
               AND    status      = 'A'
           END-EXEC

           IF SQLCODE = 0
               MOVE SQLERRD(3) TO WS-CASCADE-TERM-CNT
               ADD WS-CASCADE-TERM-CNT TO WS-TERM-CNT

               DISPLAY 'HCELIGVR: CASCADE TERMED '
                   WS-CASCADE-TERM-CNT ' DEPENDENTS FOR '
                   HV-ME-MEMBER-ID

      *        GENERATE COBRA NOTICE FOR EACH DEPENDENT
               IF WS-COBRA-ELIGIBLE
                   EXEC SQL
                       DECLARE DEP_CURSOR CURSOR FOR
                       SELECT member_id, last_name, first_name
                       FROM   T_MEMBER_ELIG
                       WHERE  subscriber_id = :HV-ME-MEMBER-ID
                       AND    relation_code <> '18'
                       AND    term_date = :E834-TERM-DATE
                   END-EXEC

                   EXEC SQL OPEN DEP_CURSOR END-EXEC

                   PERFORM UNTIL SQLCODE NOT = 0
                       EXEC SQL
                           FETCH DEP_CURSOR
                           INTO :WS-MEMBER-ID-WORK,
                                :WS-CL-MEMBER-NAME,
                                :WS-CL-MEMBER-NAME
                       END-EXEC

                       IF SQLCODE = 0
                           MOVE WS-MEMBER-ID-WORK
                               TO WS-CL-MEMBER-ID
                           PERFORM 2615-WRITE-COBRA-LETTER
                       END-IF
                   END-PERFORM

                   EXEC SQL CLOSE DEP_CURSOR END-EXEC
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       2300-DEPENDENT-ADD.
      *----------------------------------------------------------------*
      *    VALIDATE SUBSCRIBER EXISTS AND IS ACTIVE
           MOVE E834-MEMBER-ID TO HV-SUBSCRIBER-ID-IN

           EXEC SQL
               SELECT member_id, plan_code, network_cd, pcp_npi
               INTO   :HV-ME-MEMBER-ID, :HV-ME-PLAN-CODE,
                      :HV-ME-NETWORK-CD, :HV-ME-PCP-NPI
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-SUBSCRIBER-ID-IN
               AND    status    = 'A'
               AND    relation_code = '18'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'SUBSCRIBER NOT FOUND' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2300-EXIT
           END-IF

      *    CHECK DEPENDENT AGE (ACA: UNDER 26)
           PERFORM 4100-CALCULATE-AGE

           IF WS-AGE-YEARS >= WS-DEPEND-AGE-LIMIT
               IF E834-IS-DISABLED
                   AND E834-DISABILITY-DT NOT = SPACES
                   AND E834-DISABILITY-DT NOT = ZEROES
      *            DISABLED DEPENDENT EXCEPTION
                   CONTINUE
               ELSE
                   ADD 1 TO WS-834-ERROR-CNT
                   MOVE 'DEP OVER AGE LIMIT ' TO WS-RPT-MSG
                   PERFORM 8100-WRITE-DETAIL-LINE
                   GO TO 2300-EXIT
               END-IF
           END-IF

      *    VALIDATE RELATIONSHIP CODE
           IF NOT (E834-REL-SPOUSE OR E834-REL-CHILD
                   OR E834-REL-OTHER OR E834-REL-DOMESTIC
                   OR E834-REL-FOSTER OR E834-REL-WARD
                   OR E834-REL-STEPCHILD)
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'INVALID RELATION CD' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2300-EXIT
           END-IF

      *    GENERATE NEW MEMBER ID FOR DEPENDENT
           PERFORM 4090-GENERATE-MEMBER-ID

      *    INSERT DEPENDENT ELIGIBILITY
           EXEC SQL
               INSERT INTO T_MEMBER_ELIG
                   (member_id, ssn, last_name,
                    first_name, dob, gender,
                    plan_code, group_num,
                    eff_date, term_date, status,
                    subscriber_id, relation_code,
                    cobra_ind, aca_exchange_ind,
                    metal_tier, pcp_npi, network_cd,
                    disability_ind, disability_dt,
                    create_dt, create_pgm)
               VALUES
                   (:WS-NEW-MEMBER-ID,
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
                    :E834-METAL-TIER,
                    :HV-ME-PCP-NPI,
                    :HV-ME-NETWORK-CD,
                    :E834-DISABILITY-IND,
                    :E834-DISABILITY-DT,
                    :WS-CURR-DATE-8,
                    'HCELIGVR')
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-DEPEND-ADD-CNT
               MOVE WS-NEW-MEMBER-ID TO E834-MEMBER-ID
               PERFORM 4200-INITIALIZE-ACCUMULATORS
               PERFORM 4210-INITIALIZE-VISIT-LIMITS
               PERFORM 4300-FAMILY-ACCUM-ROLLUP
               MOVE 'DADD' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'DEPENDENT ADDED    ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'DEP INSERT FAILED  ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2300-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2400-DEPENDENT-REMOVE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

           EXEC SQL
               SELECT subscriber_id, plan_code, pcp_npi
               INTO   :HV-ME-SUBSCRIBER-ID, :HV-ME-PLAN-CODE,
                      :HV-ME-PCP-NPI
               FROM   T_MEMBER_ELIG
               WHERE  member_id    = :HV-ME-MEMBER-ID
               AND    status       = 'A'
               AND    relation_code <> '18'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'DEP NOT FOUND/ACTIV' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2400-EXIT
           END-IF

           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    status      = 'T',
                      term_date   = :E834-TERM-DATE,
                      term_reason = :E834-TERM-REASON-CODE,
                      update_dt   = :WS-CURR-DATE-8,
                      update_pgm  = 'HCELIGVR'
               WHERE  member_id   = :HV-ME-MEMBER-ID
               AND    status      = 'A'
               AND    relation_code <> '18'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-DEPEND-REM-CNT
               MOVE 'DREM' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               PERFORM 4300-FAMILY-ACCUM-ROLLUP

      *        COBRA ELIGIBLE IF REASON IS DIVORCE/LOSS OF STATUS
               IF E834-TERM-DIVORCE OR E834-TERM-AGE-OUT
                   SET WS-COBRA-ELIGIBLE TO TRUE
                   PERFORM 2610-GENERATE-COBRA-NOTICE
               END-IF

               MOVE 'DEPENDENT REMOVED  ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'DEP REMOVE FAILED  ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2400-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2500-PLAN-CHANGE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID
           MOVE E834-PLAN-CODE TO HV-PLAN-CODE-IN
           PERFORM 4000-VALIDATE-PLAN

           IF WS-PLAN-INVALID
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'INVALID NEW PLAN   ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2500-EXIT
           END-IF

      *    RETRIEVE CURRENT PLAN FOR AUDIT
           EXEC SQL
               SELECT plan_code, network_cd, pcp_npi, subscriber_id
               INTO   :HV-ME-PLAN-CODE, :HV-ME-NETWORK-CD,
                      :HV-ME-PCP-NPI, :HV-ME-SUBSCRIBER-ID
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'A'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'MEMBER NOT ACTIVE  ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2500-EXIT
           END-IF

           MOVE HV-ME-PLAN-CODE TO WS-AUD-OLD-VALUE

      *    CHECK IF NETWORK CHANGES - PCP MAY NEED REASSIGNMENT
           IF WS-PLN-NETWORK-CD(WS-PLAN-IDX) NOT =
              HV-ME-NETWORK-CD
               IF WS-PLN-PCP-REQD(WS-PLAN-IDX) = 'Y'
      *            CHECK IF CURRENT PCP IS IN NEW NETWORK
                   MOVE HV-ME-PCP-NPI TO HV-PCP-NPI
                   EXEC SQL
                       SELECT COUNT(*)
                       INTO   :WS-MEMBER-COUNT-WORK
                       FROM   PROVIDER_MASTER
                       WHERE  prv_npi = :HV-PCP-NPI
                       AND    prv_network_cd =
                              :WS-PLN-NETWORK-CD(WS-PLAN-IDX)
                       AND    prv_record_status = 'A'
                   END-EXEC

                   IF WS-MEMBER-COUNT-WORK = 0
      *                PCP NOT IN NEW NETWORK - AUTO REASSIGN
                       PERFORM 4060-AUTO-ASSIGN-PCP
                       MOVE 'EL0060' TO WS-ERR-CODE
                       MOVE 'PCP REASSIGNED DUE TO NETWORK CHANGE'
                           TO WS-ERR-MSG
                       MOVE 'I' TO WS-ERR-SEV
                       PERFORM 8200-WRITE-ERROR-RECORD
                   END-IF
               END-IF
           END-IF

      *    PERFORM PLAN CHANGE UPDATE
           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    plan_code  = :E834-PLAN-CODE,
                      metal_tier = :E834-METAL-TIER,
                      network_cd = :WS-PLN-NETWORK-CD(WS-PLAN-IDX),
                      pcp_npi    = :WS-PCP-NPI,
                      update_dt  = :WS-CURR-DATE-8,
                      update_pgm = 'HCELIGVR'
               WHERE  member_id  = :HV-ME-MEMBER-ID
               AND    status     = 'A'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-PLAN-CHNG-CNT
               MOVE E834-PLAN-CODE TO WS-AUD-NEW-VALUE
               MOVE 'PCHG' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL

      *        ACCUMULATOR TRANSFER LOGIC
               PERFORM 4510-TRANSFER-ACCUMULATORS

               MOVE 'PLAN CHANGED OK    ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'PLAN UPDATE FAILED ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2500-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2600-COBRA-CONTINUATION.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

      *    VERIFY MEMBER HAS RECENT TERMINATION
           EXEC SQL
               SELECT member_id, plan_code, term_date,
                      subscriber_id, coverage_level
               INTO   :HV-ME-MEMBER-ID,
                      :HV-ME-PLAN-CODE,
                      :HV-ME-TERM-DATE,
                      :HV-ME-SUBSCRIBER-ID,
                      :HV-ME-COVERAGE-LEVEL
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'T'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'NO RECENT TERM FOUND' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2600-EXIT
           END-IF

      *    DETERMINE COBRA DURATION BASED ON QUALIFYING EVENT
           MOVE 18 TO WS-COBRA-MAX-MONTHS
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 7
               IF WS-COBRA-DUR-EVENT(WS-SUB-1) =
                  E834-COBRA-QUAL-EVENT
                   MOVE WS-COBRA-DUR-MONTHS(WS-SUB-1) TO
                       WS-COBRA-MAX-MONTHS
               END-IF
           END-PERFORM

      *    CALCULATE COBRA END DATE
           MOVE E834-COBRA-EVENT-DATE TO WS-DATE-WORK
           ADD WS-COBRA-MAX-MONTHS TO WS-DATE-WORK-MM
           PERFORM UNTIL WS-DATE-WORK-MM <= 12
               SUBTRACT 12 FROM WS-DATE-WORK-MM
               ADD 1 TO WS-DATE-WORK-YYYY
           END-PERFORM
           STRING WS-DATE-WORK-YYYY WS-DATE-WORK-MM
                  WS-DATE-WORK-DD
                  DELIMITED BY SIZE INTO WS-COBRA-END-DATE

      *    CALCULATE COBRA PREMIUM (102% OF FULL PREMIUM)
           EXEC SQL
               SELECT premium_amount
               INTO   :WS-PREMIUM-FULL
               FROM   T_PLAN_BENEFIT
               WHERE  plan_code = :HV-ME-PLAN-CODE
               AND    coverage_level = :HV-ME-COVERAGE-LEVEL
           END-EXEC

           IF SQLCODE = 0
               COMPUTE WS-PREMIUM-COBRA =
                   WS-PREMIUM-FULL * 1.02
           ELSE
               MOVE ZEROES TO WS-PREMIUM-COBRA
           END-IF

      *    REACTIVATE MEMBER WITH COBRA FLAG
           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    status    = 'A',
                      cobra_ind = 'Y',
                      term_date = :WS-COBRA-END-DATE,
                      update_dt = :WS-CURR-DATE-8,
                      update_pgm = 'HCELIGVR'
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'T'
           END-EXEC

           IF SQLCODE = 0
      *        INSERT COBRA TRACKING RECORD
               MOVE E834-MEMBER-ID       TO HV-CT-MEMBER-ID
               MOVE E834-COBRA-QUAL-EVENT TO HV-CT-QUAL-EVENT
               MOVE E834-COBRA-EVENT-DATE TO HV-CT-EVENT-DATE
               MOVE E834-EFF-DATE        TO HV-CT-ELECT-DATE
               MOVE WS-COBRA-END-DATE   TO HV-CT-END-DATE
               MOVE 'A'                  TO HV-CT-STATUS
               MOVE 'N'                  TO HV-CT-SUBSIDY-IND
               MOVE WS-PREMIUM-COBRA    TO HV-CT-PREMIUM-AMT

               EXEC SQL
                   INSERT INTO T_COBRA_TRACK
                       (member_id, qual_event, event_date,
                        elect_date, end_date, status,
                        subsidy_ind, premium_amount)
                   VALUES
                       (:HV-CT-MEMBER-ID,
                        :HV-CT-QUAL-EVENT,
                        :HV-CT-EVENT-DATE,
                        :HV-CT-ELECT-DATE,
                        :HV-CT-END-DATE,
                        :HV-CT-STATUS,
                        :HV-CT-SUBSIDY-IND,
                        :HV-CT-PREMIUM-AMT)
               END-EXEC

               ADD 1 TO WS-COBRA-CNT
               MOVE 'COBR' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'COBRA ACTIVATED    ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'COBRA REACT FAILED ' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2600-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2610-GENERATE-COBRA-NOTICE.
      *----------------------------------------------------------------*
      *    GENERATE COBRA NOTIFICATION LETTER
           MOVE E834-MEMBER-ID TO WS-CL-MEMBER-ID
           STRING E834-LAST-NAME ', ' E834-FIRST-NAME
               DELIMITED BY SIZE INTO WS-CL-MEMBER-NAME
           MOVE E834-COBRA-QUAL-EVENT TO WS-CL-QUAL-EVENT-CD
           MOVE E834-COBRA-EVENT-DATE TO WS-CL-EVENT-DATE
           MOVE WS-CURR-DATE-8 TO WS-CL-NOTICE-DATE
           MOVE HV-ME-PLAN-CODE TO WS-CL-PLAN-CODE

      *    ELECTION DEADLINE = 60 DAYS FROM NOTICE
           COMPUTE WS-INTEGER-DATE-1 =
               FUNCTION INTEGER-OF-DATE(WS-CURR-DATE-8)
           ADD 60 TO WS-INTEGER-DATE-1
           COMPUTE WS-COBRA-ELECT-DEADLINE =
               FUNCTION DATE-OF-INTEGER(WS-INTEGER-DATE-1)
           MOVE WS-COBRA-ELECT-DEADLINE TO WS-CL-ELECT-DEADLINE

      *    MAP QUALIFYING EVENT TO DESCRIPTION
           EVALUATE E834-COBRA-QUAL-EVENT
               WHEN '01'
                   MOVE 'TERMINATION OF EMPLOYMENT'
                       TO WS-CL-QUAL-EVENT-DESC
               WHEN '02'
                   MOVE 'REDUCTION IN WORK HOURS'
                       TO WS-CL-QUAL-EVENT-DESC
               WHEN '03'
                   MOVE 'DIVORCE OR LEGAL SEPARATION'
                       TO WS-CL-QUAL-EVENT-DESC
               WHEN '04'
                   MOVE 'DEATH OF COVERED EMPLOYEE'
                       TO WS-CL-QUAL-EVENT-DESC
               WHEN '05'
                   MOVE 'MEDICARE ENTITLEMENT'
                       TO WS-CL-QUAL-EVENT-DESC
               WHEN '06'
                   MOVE 'LOSS OF DEPENDENT CHILD STATUS'
                       TO WS-CL-QUAL-EVENT-DESC
               WHEN '07'
                   MOVE 'EMPLOYER BANKRUPTCY'
                       TO WS-CL-QUAL-EVENT-DESC
               WHEN OTHER
                   MOVE 'OTHER QUALIFYING EVENT'
                       TO WS-CL-QUAL-EVENT-DESC
           END-EVALUATE

           PERFORM 2615-WRITE-COBRA-LETTER
           .

      *----------------------------------------------------------------*
       2615-WRITE-COBRA-LETTER.
      *----------------------------------------------------------------*
           MOVE WS-COBRA-LETTER TO COBRA-LTR-RECORD
           WRITE COBRA-LTR-RECORD
           ADD 1 TO WS-COBRA-LTR-CNT
           .

      *----------------------------------------------------------------*
       2700-OPEN-ENROLLMENT.
      *----------------------------------------------------------------*
      *    OPEN ENROLLMENT - EFFECTIVE NEXT PLAN YEAR
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID
           MOVE E834-PLAN-CODE TO HV-PLAN-CODE-IN
           PERFORM 4000-VALIDATE-PLAN

           IF WS-PLAN-INVALID
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'INVALID OE PLAN    ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2700-EXIT
           END-IF

           EXEC SQL
               SELECT member_id, plan_code, subscriber_id
               INTO   :HV-ME-MEMBER-ID, :HV-ME-PLAN-CODE,
                      :HV-ME-SUBSCRIBER-ID
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'A'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'MEMBER NOT ACTIVE  ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2700-EXIT
           END-IF

           MOVE HV-ME-PLAN-CODE TO WS-AUD-OLD-VALUE

      *    TERMINATE CURRENT PLAN AT END OF PLAN YEAR
           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    term_date  = :WS-PLAN-YEAR-END,
                      update_dt  = :WS-CURR-DATE-8,
                      update_pgm = 'HCELIGVR'
               WHERE  member_id  = :HV-ME-MEMBER-ID
               AND    status     = 'A'
               AND    plan_code  = :HV-ME-PLAN-CODE
           END-EXEC

      *    INSERT NEW ENROLLMENT EFFECTIVE NEXT PLAN YEAR
           EXEC SQL
               INSERT INTO T_MEMBER_ELIG
                   (member_id, ssn, last_name,
                    first_name, dob, gender,
                    plan_code, group_num,
                    eff_date, term_date, status,
                    subscriber_id, relation_code,
                    cobra_ind, aca_exchange_ind,
                    metal_tier, network_cd,
                    create_dt, create_pgm)
               SELECT member_id, ssn, last_name,
                      first_name, dob, gender,
                      :E834-PLAN-CODE, group_num,
                      :E834-EFF-DATE, '99991231', 'P',
                      subscriber_id, relation_code,
                      cobra_ind,
                      :E834-ACA-EXCHANGE-IND,
                      :E834-METAL-TIER,
                      :WS-PLN-NETWORK-CD(WS-PLAN-IDX),
                      :WS-CURR-DATE-8,
                      'HCELIGVR'
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

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2700-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2800-RETROACTIVE-CHANGE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID
           SET WS-IS-RETRO TO TRUE

      *    UPDATE ELIGIBILITY WITH RETROACTIVE EFFECTIVE DATE
           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    eff_date   = :E834-EFF-DATE,
                      plan_code  = :E834-PLAN-CODE,
                      update_dt  = :WS-CURR-DATE-8,
                      update_pgm = 'HCELIGVR'
               WHERE  member_id  = :HV-ME-MEMBER-ID
               AND    status IN ('A', 'T')
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-RETRO-CHG-CNT
               MOVE 'RETR' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL

      *        RECALCULATE ALL ACCUMULATORS FROM RETRO DATE
               PERFORM 6000-RETRO-ACCUM-RECALC

      *        FLAG AFFECTED CLAIMS FOR REPROCESSING
               PERFORM 6100-FLAG-CLAIMS-REPROCESS

               MOVE 'RETRO CHANGE OK    ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'RETRO UPDATE FAILED' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2810-REACTIVATION.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

           EXEC SQL
               SELECT plan_code, term_date, term_reason
               INTO   :HV-ME-PLAN-CODE, :HV-ME-TERM-DATE,
                      :WS-AUD-REASON-CODE
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'T'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'NO TERMED RECORD   ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2810-EXIT
           END-IF

      *    CANNOT REACTIVATE IF TERMED FOR FRAUD
           IF WS-AUD-REASON-CODE = '003'
               MOVE 'EL0080' TO WS-ERR-CODE
               MOVE 'CANNOT REACTIVATE - TERMED FOR FRAUD'
                   TO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               MOVE 'FRAUD - NO REACTIVE' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2810-EXIT
           END-IF

           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    status     = 'A',
                      term_date  = '99991231',
                      term_reason = NULL,
                      update_dt  = :WS-CURR-DATE-8,
                      update_pgm = 'HCELIGVR'
               WHERE  member_id  = :HV-ME-MEMBER-ID
               AND    status     = 'T'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-REACTIVATION-CNT
               MOVE 'REAC' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'REACTIVATED OK     ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'REACTIVATION FAILED' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2810-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2820-ADDRESS-CHANGE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

           EXEC SQL
               UPDATE PATIENT_MASTER
               SET    pat_addr_line_1 = :E834-ADDRESS-LINE-1,
                      pat_addr_line_2 = :E834-ADDRESS-LINE-2,
                      pat_city        = :E834-CITY,
                      pat_state       = :E834-STATE,
                      pat_zip         = :E834-ZIP,
                      pat_home_phone  = :E834-PHONE,
                      pat_email       = :E834-EMAIL,
                      pat_last_upd_dt = :WS-CURR-DATE-8,
                      pat_last_upd_user = 'HCELIGVR'
               WHERE  pat_mrn = :HV-ME-MEMBER-ID
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-ADDRESS-CHG-CNT
               MOVE 'ADDR' TO WS-AUD-ACTION
               MOVE 'PAT_ADDRESS' TO WS-AUD-FIELD-NAME
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'ADDRESS UPDATED    ' TO WS-RPT-MSG
           ELSE
               MOVE 'ADDRESS UPD FAILED ' TO WS-RPT-MSG
               ADD 1 TO WS-834-ERROR-CNT
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2830-PCP-CHANGE.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID
           MOVE E834-PCP-NPI TO WS-PCP-NPI

      *    VALIDATE NEW PCP
           IF E834-PCP-NPI NOT = SPACES
               PERFORM 4050-VALIDATE-PCP
               IF WS-PCP-VALID
                   EXEC SQL
                       SELECT pcp_npi
                       INTO   :HV-ME-PCP-NPI
                       FROM   T_MEMBER_ELIG
                       WHERE  member_id = :HV-ME-MEMBER-ID
                       AND    status    = 'A'
                   END-EXEC

                   IF SQLCODE = 0
                       MOVE HV-ME-PCP-NPI TO WS-AUD-OLD-VALUE

      *                DECREMENT OLD PCP PANEL
                       IF HV-ME-PCP-NPI NOT = SPACES
                           EXEC SQL
                               UPDATE T_PCP_PANEL
                               SET    panel_current =
                                      panel_current - 1
                               WHERE  pcp_npi = :HV-ME-PCP-NPI
                               AND    panel_current > 0
                           END-EXEC
                       END-IF

      *                UPDATE TO NEW PCP
                       EXEC SQL
                           UPDATE T_MEMBER_ELIG
                           SET    pcp_npi    = :E834-PCP-NPI,
                                  update_dt  = :WS-CURR-DATE-8,
                                  update_pgm = 'HCELIGVR'
                           WHERE  member_id  = :HV-ME-MEMBER-ID
                           AND    status     = 'A'
                       END-EXEC

                       IF SQLCODE = 0
      *                    INCREMENT NEW PCP PANEL
                           EXEC SQL
                               UPDATE T_PCP_PANEL
                               SET    panel_current =
                                      panel_current + 1
                               WHERE  pcp_npi = :E834-PCP-NPI
                           END-EXEC

                           ADD 1 TO WS-PCP-CHG-CNT
                           MOVE E834-PCP-NPI TO WS-AUD-NEW-VALUE
                           MOVE 'PCPC' TO WS-AUD-ACTION
                           PERFORM 7000-WRITE-AUDIT-TRAIL
                           MOVE 'PCP CHANGED OK     ' TO WS-RPT-MSG
                       ELSE
                           MOVE 'PCP UPDATE FAILED  ' TO WS-RPT-MSG
                           ADD 1 TO WS-834-ERROR-CNT
                       END-IF
                   END-IF
               ELSE
                   MOVE 'PCP NPI INVALID    ' TO WS-RPT-MSG
                   ADD 1 TO WS-834-ERROR-CNT
               END-IF
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *----------------------------------------------------------------*
       2840-NEWBORN-ENROLLMENT.
      *----------------------------------------------------------------*
      *    AUTO-ENROLL NEWBORN RETROACTIVE TO DATE OF BIRTH
           SET WS-IS-NEWBORN TO TRUE

      *    FIND MOTHER'S ENROLLMENT
           MOVE E834-NEWBORN-MOTHER-ID TO HV-SUBSCRIBER-ID-IN

           EXEC SQL
               SELECT subscriber_id, plan_code, group_num,
                      network_cd, pcp_npi
               INTO   :HV-ME-SUBSCRIBER-ID, :HV-ME-PLAN-CODE,
                      :HV-ME-GROUP-NUM, :HV-ME-NETWORK-CD,
                      :HV-ME-PCP-NPI
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-SUBSCRIBER-ID-IN
               AND    status    = 'A'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'MOTHER NOT FOUND   ' TO WS-RPT-MSG
               PERFORM 8100-WRITE-DETAIL-LINE
               GO TO 2840-EXIT
           END-IF

      *    GENERATE MEMBER ID FOR NEWBORN
           PERFORM 4090-GENERATE-MEMBER-ID

      *    EFF DATE = DOB (RETROACTIVE)
           MOVE E834-DOB TO E834-EFF-DATE

           EXEC SQL
               INSERT INTO T_MEMBER_ELIG
                   (member_id, ssn, last_name,
                    first_name, dob, gender,
                    plan_code, group_num,
                    eff_date, term_date, status,
                    subscriber_id, relation_code,
                    cobra_ind, network_cd, pcp_npi,
                    create_dt, create_pgm)
               VALUES
                   (:WS-NEW-MEMBER-ID,
                    :E834-MEMBER-SSN,
                    :E834-LAST-NAME,
                    :E834-FIRST-NAME,
                    :E834-DOB,
                    :E834-GENDER,
                    :HV-ME-PLAN-CODE,
                    :HV-ME-GROUP-NUM,
                    :E834-EFF-DATE,
                    '99991231', 'A',
                    :HV-ME-SUBSCRIBER-ID,
                    '19',
                    'N',
                    :HV-ME-NETWORK-CD,
                    :HV-ME-PCP-NPI,
                    :WS-CURR-DATE-8,
                    'HCELIGVR')
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-NEWBORN-CNT
               MOVE WS-NEW-MEMBER-ID TO E834-MEMBER-ID
               PERFORM 4200-INITIALIZE-ACCUMULATORS
               PERFORM 4210-INITIALIZE-VISIT-LIMITS
               PERFORM 4300-FAMILY-ACCUM-ROLLUP
               MOVE 'NEWB' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'NEWBORN ENROLLED   ' TO WS-RPT-MSG
           ELSE
               ADD 1 TO WS-834-ERROR-CNT
               MOVE 'NEWBORN ENRL FAILED' TO WS-RPT-MSG
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2840-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2850-QMCSO-PROCESSING.
      *----------------------------------------------------------------*
      *    QUALIFIED MEDICAL CHILD SUPPORT ORDER
           SET WS-HAS-QMCSO TO TRUE

      *    QMCSO REQUIRES ENROLLMENT OF CHILD REGARDLESS OF
      *    SUBSCRIBER ENROLLMENT STATUS (COURT ORDER)
           MOVE E834-MEMBER-ID TO HV-SUBSCRIBER-ID-IN

      *    FIND PARENT (NCP - NON-CUSTODIAL PARENT)
           EXEC SQL
               SELECT plan_code, group_num, network_cd
               INTO   :HV-ME-PLAN-CODE, :HV-ME-GROUP-NUM,
                      :HV-ME-NETWORK-CD
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :HV-SUBSCRIBER-ID-IN
               AND    relation_code = '18'
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'EL0090' TO WS-ERR-CODE
               MOVE 'QMCSO: NCP NOT FOUND IN SYSTEM'
                   TO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               GO TO 2850-EXIT
           END-IF

      *    ENROLL CHILD PER COURT ORDER
           PERFORM 4090-GENERATE-MEMBER-ID

           EXEC SQL
               INSERT INTO T_MEMBER_ELIG
                   (member_id, last_name, first_name, dob, gender,
                    plan_code, group_num, eff_date, term_date,
                    status, subscriber_id, relation_code,
                    network_cd, qmcso_order_no,
                    create_dt, create_pgm)
               VALUES
                   (:WS-NEW-MEMBER-ID,
                    :E834-LAST-NAME, :E834-FIRST-NAME,
                    :E834-DOB, :E834-GENDER,
                    :HV-ME-PLAN-CODE, :HV-ME-GROUP-NUM,
                    :E834-EFF-DATE, '99991231', 'A',
                    :HV-SUBSCRIBER-ID-IN, '19',
                    :HV-ME-NETWORK-CD,
                    :E834-QMCSO-COURT-ORDER,
                    :WS-CURR-DATE-8, 'HCELIGVR')
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-QMCSO-CNT
               MOVE WS-NEW-MEMBER-ID TO E834-MEMBER-ID
               PERFORM 4200-INITIALIZE-ACCUMULATORS
               MOVE 'QMCS' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'QMCSO ENROLLED     ' TO WS-RPT-MSG
           ELSE
               MOVE 'QMCSO ENRL FAILED  ' TO WS-RPT-MSG
               ADD 1 TO WS-834-ERROR-CNT
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .
       2850-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2860-DISABILITY-EXTENSION.
      *----------------------------------------------------------------*
      *    EXTEND DEPENDENT COVERAGE BEYOND AGE 26 FOR DISABLED
           MOVE E834-MEMBER-ID TO HV-ME-MEMBER-ID

           EXEC SQL
               UPDATE T_MEMBER_ELIG
               SET    disability_ind = 'Y',
                      disability_dt  = :E834-DISABILITY-DT,
                      term_date      = '99991231',
                      update_dt      = :WS-CURR-DATE-8,
                      update_pgm     = 'HCELIGVR'
               WHERE  member_id      = :HV-ME-MEMBER-ID
               AND    relation_code  <> '18'
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-DISABILITY-CNT
               MOVE 'DISA' TO WS-AUD-ACTION
               PERFORM 7000-WRITE-AUDIT-TRAIL
               MOVE 'DISABILITY EXTENDED ' TO WS-RPT-MSG
           ELSE
               MOVE 'DISAB EXT FAILED   ' TO WS-RPT-MSG
               ADD 1 TO WS-834-ERROR-CNT
           END-IF

           PERFORM 8100-WRITE-DETAIL-LINE
           .

      *================================================================*
      * 270/271 ELIGIBILITY VERIFICATION
      *================================================================*

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

      *    STEP 1: CHECK DATE-OF-SERVICE ELIGIBILITY
           EXEC SQL
               SELECT me.plan_code, me.eff_date, me.term_date,
                      me.status, me.cobra_ind, me.subscriber_id,
                      me.relation_code, me.pcp_npi, me.network_cd,
                      me.aca_exchange_ind, me.grandfathered,
                      me.plan_type, me.disability_ind
               INTO   :HV-ME-PLAN-CODE, :HV-ME-EFF-DATE,
                      :HV-ME-TERM-DATE, :HV-ME-STATUS,
                      :HV-ME-COBRA-IND, :HV-ME-SUBSCRIBER-ID,
                      :HV-ME-RELATION-CODE, :HV-ME-PCP-NPI,
                      :HV-ME-NETWORK-CD, :HV-ME-ACA-EXCHANGE-IND,
                      :HV-ME-GRANDFATHERED, :HV-ME-PLAN-TYPE,
                      :HV-ME-DISABILITY-IND
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
               MOVE HV-ME-NETWORK-CD TO WS-271-NETWORK-CD
               MOVE HV-ME-GRANDFATHERED TO WS-271-GRANDFATHERED
               MOVE HV-ME-PLAN-TYPE TO WS-271-PLAN-TYPE

               IF HV-ME-COBRA-IND = 'Y'
                   SET WS-271-COBRA TO TRUE
                   PERFORM 3200-CHECK-GRACE-PERIOD
               ELSE
                   SET WS-271-ACTIVE TO TRUE
               END-IF

      *        STEP 2: LOAD PLAN BENEFITS
               PERFORM 3300-LOAD-PLAN-BENEFITS

      *        STEP 3: RETRIEVE ACCUMULATORS
               PERFORM 3400-GET-ACCUMULATORS

      *        STEP 4: GET VISIT LIMITS
               PERFORM 3500-GET-VISIT-LIMITS

      *        STEP 5: CHECK PCP ASSIGNMENT
               PERFORM 3600-CHECK-PCP-ASSIGNMENT

      *        STEP 6: CHECK COB / OTHER INSURANCE
               PERFORM 3700-CHECK-COB-INFO

      *        STEP 7: CHECK SPECIAL PROGRAMS
               PERFORM 3800-CHECK-SPECIAL-PROGRAMS

      *        STEP 8: CHECK WAITING PERIOD / PREEXISTING
               PERFORM 3850-CHECK-WAITING-PREEXIST

      *        STEP 9: CHECK PLAN YEAR RESET
               PERFORM 4500-PLAN-YEAR-RESET-CHECK

               MOVE SPACES TO WS-271-REJECT-REASON
           ELSE
      *        MEMBER NOT FOUND - CHECK RUN-OUT PERIOD
               PERFORM 3150-CHECK-RUNOUT-PERIOD

               IF NOT WS-ELIG-ACTIVE
                   SET WS-ELIG-INACTIVE TO TRUE
                   SET WS-271-INACTIVE  TO TRUE
                   MOVE '072' TO WS-271-REJECT-REASON
                   MOVE 'NOT ELIGIBLE ON DOS'
                       TO WS-271-REJECT-MSG
               END-IF
           END-IF

      *    WRITE 271 RESPONSE
           MOVE WS-271-RESPONSE TO ELIG-271-RECORD
           WRITE ELIG-271-RECORD
           ADD 1 TO WS-271-WRITE-CNT

      *    LOG INQUIRY
           PERFORM 7100-LOG-ELIGIBILITY-INQUIRY
           .

      *----------------------------------------------------------------*
       3150-CHECK-RUNOUT-PERIOD.
      *----------------------------------------------------------------*
      *    CHECK IF CLAIM FALLS WITHIN RUN-OUT PERIOD
      *    (DOS BEFORE TERM DATE, CLAIM RECEIVED WITHIN 90 DAYS)
           EXEC SQL
               SELECT me.plan_code, me.eff_date, me.term_date,
                      me.status, me.subscriber_id,
                      me.pcp_npi, me.network_cd
               INTO   :HV-ME-PLAN-CODE, :HV-ME-EFF-DATE,
                      :HV-ME-TERM-DATE, :HV-ME-STATUS,
                      :HV-ME-SUBSCRIBER-ID,
                      :HV-ME-PCP-NPI, :HV-ME-NETWORK-CD
               FROM   T_MEMBER_ELIG me
               WHERE  me.member_id = :HV-ME-MEMBER-ID
               AND    me.status    = 'T'
               AND    me.eff_date  <= :HV-SERVICE-DATE
               AND    me.term_date >= :HV-SERVICE-DATE
           END-EXEC

           IF SQLCODE = 0
      *        DOS IS BEFORE TERM DATE - CHECK IF WITHIN RUNOUT
               COMPUTE WS-INTEGER-DATE-1 =
                   FUNCTION INTEGER-OF-DATE(HV-ME-TERM-DATE)
               COMPUTE WS-INTEGER-DATE-2 =
                   FUNCTION INTEGER-OF-DATE(WS-CURR-DATE-8)
               COMPUTE WS-DAYS-DIFF =
                   WS-INTEGER-DATE-2 - WS-INTEGER-DATE-1

               IF WS-DAYS-DIFF <= WS-RUNOUT-DAYS
                   SET WS-ELIG-ACTIVE TO TRUE
                   SET WS-271-RUNOUT TO TRUE
                   MOVE HV-ME-PLAN-CODE TO WS-271-PLAN-CODE
                   MOVE HV-ME-EFF-DATE  TO WS-271-EFF-DATE
                   MOVE HV-ME-TERM-DATE TO WS-271-TERM-DATE
                   MOVE HV-ME-NETWORK-CD TO WS-271-NETWORK-CD

                   PERFORM 3300-LOAD-PLAN-BENEFITS
                   PERFORM 3400-GET-ACCUMULATORS
               END-IF
           END-IF
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
               SET WS-271-GRACE TO TRUE
           ELSE
               MOVE 31 TO WS-GRACE-PERIOD-DAYS
           END-IF

           SET WS-IN-GRACE-PERIOD TO TRUE
           .

      *----------------------------------------------------------------*
       3300-LOAD-PLAN-BENEFITS.
      *----------------------------------------------------------------*
      *    LOOK UP PLAN IN MEMORY TABLE
           MOVE 0 TO WS-PLAN-IDX
           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > WS-PLAN-TABLE-COUNT
               OR WS-PLAN-IDX > 0
               IF WS-PLN-PLAN-CODE(WS-SUB-1) = HV-ME-PLAN-CODE
                   MOVE WS-SUB-1 TO WS-PLAN-IDX
               END-IF
           END-PERFORM

           IF WS-PLAN-IDX > 0
      *        POPULATE 271 BENEFIT FIELDS
               MOVE WS-PLN-COPAY-PCP(WS-PLAN-IDX)
                   TO WS-271-COPAY-PCP
               MOVE WS-PLN-COPAY-SPEC(WS-PLAN-IDX)
                   TO WS-271-COPAY-SPEC
               MOVE WS-PLN-COPAY-ER(WS-PLAN-IDX)
                   TO WS-271-COPAY-ER
               MOVE WS-PLN-COPAY-MH(WS-PLAN-IDX)
                   TO WS-271-COPAY-MH
               MOVE WS-PLN-COINSUR-IN(WS-PLAN-IDX)
                   TO WS-271-COINSURANCE-IN
               MOVE WS-PLN-COINSUR-OON(WS-PLAN-IDX)
                   TO WS-271-COINSURANCE-OON

               IF WS-PLN-PCP-REQD(WS-PLAN-IDX) = 'Y'
                   MOVE 'Y' TO WS-271-PCP-REQUIRED
               ELSE
                   MOVE 'N' TO WS-271-PCP-REQUIRED
               END-IF

               IF WS-PLN-REFERRAL-RQ(WS-PLAN-IDX) = 'Y'
                   MOVE 'Y' TO WS-271-REFERRAL-REQD
               ELSE
                   MOVE 'N' TO WS-271-REFERRAL-REQD
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       3400-GET-ACCUMULATORS.
      *----------------------------------------------------------------*
      *    RETRIEVE INDIVIDUAL ACCUMULATORS
           MOVE WS-CURR-YYYY TO HV-MA-PLAN-YEAR

           EXEC SQL
               SELECT ded_used_in, ded_used_oon,
                      oop_used_in, oop_used_oon,
                      lifetime_used, annual_used,
                      carryover_credit
               INTO   :HV-MA-DED-USED-IN, :HV-MA-DED-USED-OON,
                      :HV-MA-OOP-USED-IN, :HV-MA-OOP-USED-OON,
                      :HV-MA-LIFETIME-USED, :HV-MA-ANNUAL-USED,
                      :HV-MA-CARRYOVER
               FROM   T_MEMBER_ACCUM
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    plan_year  = :HV-MA-PLAN-YEAR
           END-EXEC

           IF SQLCODE = 0 AND WS-PLAN-IDX > 0
      *        CALCULATE REMAINING DEDUCTIBLE (IN-NETWORK)
               COMPUTE WS-271-DEDUCT-REM-IN =
                   WS-PLN-IND-DED-IN(WS-PLAN-IDX)
                   - HV-MA-DED-USED-IN
                   + HV-MA-CARRYOVER
               IF WS-271-DEDUCT-REM-IN < 0
                   MOVE ZEROES TO WS-271-DEDUCT-REM-IN
               END-IF
               MOVE HV-MA-DED-USED-IN TO WS-271-DEDUCT-IND-IN

      *        CALCULATE REMAINING DEDUCTIBLE (OUT-OF-NETWORK)
               COMPUTE WS-271-DEDUCT-REM-OON =
                   WS-PLN-IND-DED-OON(WS-PLAN-IDX)
                   - HV-MA-DED-USED-OON
               IF WS-271-DEDUCT-REM-OON < 0
                   MOVE ZEROES TO WS-271-DEDUCT-REM-OON
               END-IF
               MOVE HV-MA-DED-USED-OON TO WS-271-DEDUCT-IND-OON

      *        CALCULATE REMAINING OOP (IN-NETWORK)
               COMPUTE WS-271-OOP-REM-IN =
                   WS-PLN-IND-OOP-IN(WS-PLAN-IDX)
                   - HV-MA-OOP-USED-IN
               IF WS-271-OOP-REM-IN < 0
                   MOVE ZEROES TO WS-271-OOP-REM-IN
               END-IF
               MOVE HV-MA-OOP-USED-IN TO WS-271-OOP-IND-IN

      *        LIFETIME REMAINING (GRANDFATHERED PLANS)
               IF WS-PLN-GRANDFATHRD(WS-PLAN-IDX) = 'Y'
                   COMPUTE WS-271-LIFETIME-REMAIN =
                       WS-PLN-LIFETIME-MX(WS-PLAN-IDX)
                       - HV-MA-LIFETIME-USED
                   IF WS-271-LIFETIME-REMAIN < 0
                       MOVE ZEROES TO WS-271-LIFETIME-REMAIN
                   END-IF
               ELSE
                   MOVE 999999999.99 TO WS-271-LIFETIME-REMAIN
               END-IF

      *        ANNUAL MAX REMAINING
               IF WS-PLN-ANNUAL-MAX(WS-PLAN-IDX) > 0
                   COMPUTE WS-271-ANNUAL-REMAIN =
                       WS-PLN-ANNUAL-MAX(WS-PLAN-IDX)
                       - HV-MA-ANNUAL-USED
                   IF WS-271-ANNUAL-REMAIN < 0
                       MOVE ZEROES TO WS-271-ANNUAL-REMAIN
                   END-IF
               ELSE
                   MOVE 999999999.99 TO WS-271-ANNUAL-REMAIN
               END-IF

      *        GET FAMILY ACCUMULATORS
               MOVE HV-ME-SUBSCRIBER-ID TO HV-FA-SUBSCRIBER-ID
               MOVE WS-CURR-YYYY TO HV-FA-PLAN-YEAR

               EXEC SQL
                   SELECT ded_used_in, oop_used_in
                   INTO   :HV-FA-DED-USED-IN, :HV-FA-OOP-USED-IN
                   FROM   T_FAMILY_ACCUM
                   WHERE  subscriber_id = :HV-FA-SUBSCRIBER-ID
                   AND    plan_year     = :HV-FA-PLAN-YEAR
               END-EXEC

               IF SQLCODE = 0
                   MOVE HV-FA-DED-USED-IN TO WS-271-DEDUCT-FAM-IN
                   COMPUTE WS-271-DEDUCT-FAM-REM =
                       WS-PLN-FAM-DED-IN(WS-PLAN-IDX)
                       - HV-FA-DED-USED-IN
                   IF WS-271-DEDUCT-FAM-REM < 0
                       MOVE ZEROES TO WS-271-DEDUCT-FAM-REM
                   END-IF

                   MOVE HV-FA-OOP-USED-IN TO WS-271-OOP-FAM-IN
                   COMPUTE WS-271-OOP-FAM-REM =
                       WS-PLN-FAM-OOP-IN(WS-PLAN-IDX)
                       - HV-FA-OOP-USED-IN
                   IF WS-271-OOP-FAM-REM < 0
                       MOVE ZEROES TO WS-271-OOP-FAM-REM
                   END-IF
               END-IF
           ELSE
      *        NO ACCUMULATORS FOR CURRENT YEAR - RETURN FULL BENEFIT
               IF WS-PLAN-IDX > 0
                   MOVE WS-PLN-IND-DED-IN(WS-PLAN-IDX)
                       TO WS-271-DEDUCT-REM-IN
                   MOVE WS-PLN-IND-DED-OON(WS-PLAN-IDX)
                       TO WS-271-DEDUCT-REM-OON
                   MOVE WS-PLN-IND-OOP-IN(WS-PLAN-IDX)
                       TO WS-271-OOP-REM-IN
                   MOVE WS-PLN-FAM-DED-IN(WS-PLAN-IDX)
                       TO WS-271-DEDUCT-FAM-REM
                   MOVE WS-PLN-FAM-OOP-IN(WS-PLAN-IDX)
                       TO WS-271-OOP-FAM-REM
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       3500-GET-VISIT-LIMITS.
      *----------------------------------------------------------------*
           IF WS-PLAN-IDX = 0
               GO TO 3500-EXIT
           END-IF

           MOVE HV-ME-MEMBER-ID TO HV-VL-MEMBER-ID
           MOVE WS-CURR-YYYY TO HV-VL-PLAN-YEAR

           EXEC SQL
               SELECT pt_used, ot_used, mh_used,
                      chiro_used, speech_used, sud_used
               INTO   :HV-VL-PT-USED, :HV-VL-OT-USED,
                      :HV-VL-MH-USED, :HV-VL-CHIRO-USED,
                      :HV-VL-SPEECH-USED, :HV-VL-SUD-USED
               FROM   T_VISIT_LIMITS
               WHERE  member_id = :HV-VL-MEMBER-ID
               AND    plan_year = :HV-VL-PLAN-YEAR
           END-EXEC

           IF SQLCODE = 0
               IF WS-PLN-PT-VISITS(WS-PLAN-IDX) > 0
                   COMPUTE WS-271-PT-REMAIN =
                       WS-PLN-PT-VISITS(WS-PLAN-IDX)
                       - HV-VL-PT-USED
                   IF WS-271-PT-REMAIN < 0
                       MOVE ZEROES TO WS-271-PT-REMAIN
                   END-IF
               ELSE
                   MOVE 999 TO WS-271-PT-REMAIN
               END-IF

               IF WS-PLN-OT-VISITS(WS-PLAN-IDX) > 0
                   COMPUTE WS-271-OT-REMAIN =
                       WS-PLN-OT-VISITS(WS-PLAN-IDX)
                       - HV-VL-OT-USED
                   IF WS-271-OT-REMAIN < 0
                       MOVE ZEROES TO WS-271-OT-REMAIN
                   END-IF
               ELSE
                   MOVE 999 TO WS-271-OT-REMAIN
               END-IF

      *        MENTAL HEALTH PARITY: MH VISITS CANNOT BE MORE
      *        RESTRICTIVE THAN MED/SURG (USE UNLIMITED IF
      *        MED/SURG IS UNLIMITED)
               IF WS-PLN-MH-VISITS(WS-PLAN-IDX) > 0
                   COMPUTE WS-271-MH-REMAIN =
                       WS-PLN-MH-VISITS(WS-PLAN-IDX)
                       - HV-VL-MH-USED
                   IF WS-271-MH-REMAIN < 0
                       MOVE ZEROES TO WS-271-MH-REMAIN
                   END-IF
               ELSE
                   MOVE 999 TO WS-271-MH-REMAIN
               END-IF

               IF WS-PLN-CHIRO-VISIT(WS-PLAN-IDX) > 0
                   COMPUTE WS-271-CHIRO-REMAIN =
                       WS-PLN-CHIRO-VISIT(WS-PLAN-IDX)
                       - HV-VL-CHIRO-USED
                   IF WS-271-CHIRO-REMAIN < 0
                       MOVE ZEROES TO WS-271-CHIRO-REMAIN
                   END-IF
               ELSE
                   MOVE 999 TO WS-271-CHIRO-REMAIN
               END-IF

               IF WS-PLN-SPEECH-VIS(WS-PLAN-IDX) > 0
                   COMPUTE WS-271-SPEECH-REMAIN =
                       WS-PLN-SPEECH-VIS(WS-PLAN-IDX)
                       - HV-VL-SPEECH-USED
                   IF WS-271-SPEECH-REMAIN < 0
                       MOVE ZEROES TO WS-271-SPEECH-REMAIN
                   END-IF
               ELSE
                   MOVE 999 TO WS-271-SPEECH-REMAIN
               END-IF

               IF WS-PLN-SUD-VISITS(WS-PLAN-IDX) > 0
                   COMPUTE WS-271-SUD-REMAIN =
                       WS-PLN-SUD-VISITS(WS-PLAN-IDX)
                       - HV-VL-SUD-USED
                   IF WS-271-SUD-REMAIN < 0
                       MOVE ZEROES TO WS-271-SUD-REMAIN
                   END-IF
               ELSE
                   MOVE 999 TO WS-271-SUD-REMAIN
               END-IF
           ELSE
      *        NO VISITS USED YET
               MOVE WS-PLN-PT-VISITS(WS-PLAN-IDX)
                   TO WS-271-PT-REMAIN
               MOVE WS-PLN-OT-VISITS(WS-PLAN-IDX)
                   TO WS-271-OT-REMAIN
               MOVE WS-PLN-MH-VISITS(WS-PLAN-IDX)
                   TO WS-271-MH-REMAIN
               MOVE WS-PLN-CHIRO-VISIT(WS-PLAN-IDX)
                   TO WS-271-CHIRO-REMAIN
               MOVE WS-PLN-SPEECH-VIS(WS-PLAN-IDX)
                   TO WS-271-SPEECH-REMAIN
               MOVE WS-PLN-SUD-VISITS(WS-PLAN-IDX)
                   TO WS-271-SUD-REMAIN
           END-IF
           .
       3500-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3600-CHECK-PCP-ASSIGNMENT.
      *----------------------------------------------------------------*
           MOVE HV-ME-PCP-NPI TO WS-271-PCP-NPI

           IF HV-ME-PCP-NPI NOT = SPACES
               EXEC SQL
                   SELECT prv_last_name || ', ' || prv_first_name
                   INTO   :WS-271-PCP-NAME
                   FROM   PROVIDER_MASTER
                   WHERE  prv_npi = :HV-ME-PCP-NPI
                   AND    prv_record_status = 'A'
               END-EXEC

               IF SQLCODE NOT = 0
                   MOVE '** PCP NOT ACTIVE **' TO WS-271-PCP-NAME
               END-IF
           END-IF

      *    CHECK SELF-REFERRAL EXCEPTIONS
           IF WS-271-REFERRAL-REQD = 'Y'
               EVALUATE TRUE
                   WHEN E270-SVC-EMERGENCY
                       SET WS-SELF-REFERRAL-OK TO TRUE
                   WHEN E270-SVC-MENTAL
                       SET WS-SELF-REFERRAL-OK TO TRUE
                   WHEN E270-SVC-SUBSTANCE
                       SET WS-SELF-REFERRAL-OK TO TRUE
               END-EVALUATE
           END-IF
           .

      *----------------------------------------------------------------*
       3700-CHECK-COB-INFO.
      *----------------------------------------------------------------*
           MOVE HV-ME-MEMBER-ID TO HV-COB-MEMBER-ID

           EXEC SQL
               SELECT other_payer_cd, other_member_id,
                      other_group_no, cob_sequence,
                      medicare_flag, medicare_part_a,
                      medicare_part_b, medicare_part_d,
                      medicaid_flag, dual_eligible, msp_code
               INTO   :HV-COB-OTHER-PAYER,
                      :HV-COB-OTHER-MBR-ID,
                      :HV-COB-OTHER-GROUP,
                      :HV-COB-SEQ,
                      :HV-COB-MCARE-FLAG,
                      :HV-COB-MCARE-PART-A,
                      :HV-COB-MCARE-PART-B,
                      :HV-COB-MCARE-PART-D,
                      :HV-COB-MCAID-FLAG,
                      :HV-COB-DUAL-ELIG,
                      :HV-COB-MSP-CODE
               FROM   T_COB_OTHER_INS
               WHERE  member_id = :HV-COB-MEMBER-ID
               AND    eff_date <= :HV-SERVICE-DATE
               AND    (term_date >= :HV-SERVICE-DATE
                       OR term_date = '99991231')
           END-EXEC

           IF SQLCODE = 0
               MOVE 'Y' TO WS-271-COB-IND
               MOVE HV-COB-OTHER-PAYER TO WS-271-COB-PAYER
               MOVE HV-COB-SEQ TO WS-271-COB-SEQ
               MOVE HV-COB-MCARE-FLAG TO WS-271-MCARE-IND
               MOVE HV-COB-MCAID-FLAG TO WS-271-MCAID-IND
           ELSE
               MOVE 'N' TO WS-271-COB-IND
           END-IF
           .

      *----------------------------------------------------------------*
       3800-CHECK-SPECIAL-PROGRAMS.
      *----------------------------------------------------------------*
           INITIALIZE WS-SPECIAL-PGMS
           MOVE SPACES TO WS-271-SPECIAL-PGMS

           EXEC SQL
               DECLARE SPG_CURSOR CURSOR FOR
               SELECT program_cd
               FROM   T_SPECIAL_PROGRAMS
               WHERE  member_id = :HV-ME-MEMBER-ID
               AND    status    = 'A'
           END-EXEC

           EXEC SQL OPEN SPG_CURSOR END-EXEC

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH SPG_CURSOR
                   INTO :HV-SPG-PROGRAM-CD
               END-EXEC

               IF SQLCODE = 0
                   EVALUATE HV-SPG-PROGRAM-CD
                       WHEN 'DMDB'
                           SET WS-IN-DM-DIABETES TO TRUE
                       WHEN 'DMCH'
                           SET WS-IN-DM-CHF TO TRUE
                       WHEN 'DMCO'
                           SET WS-IN-DM-COPD TO TRUE
                       WHEN 'DMAS'
                           SET WS-IN-DM-ASTHMA TO TRUE
                       WHEN 'CMGT'
                           SET WS-IN-CASE-MGMT TO TRUE
                       WHEN 'MATR'
                           SET WS-IN-MATERNITY TO TRUE
                       WHEN 'TRNS'
                           SET WS-IN-TRANSPLANT TO TRUE
                       WHEN 'WELL'
                           SET WS-IN-WELLNESS TO TRUE
                       WHEN 'CCMG'
                           SET WS-IN-CHRONIC-CARE TO TRUE
                   END-EVALUATE
               END-IF
           END-PERFORM

           EXEC SQL CLOSE SPG_CURSOR END-EXEC

      *    BUILD SPECIAL PROGRAMS STRING FOR 271
           STRING
               WS-SP-DM-DIABETES
               WS-SP-DM-CHF
               WS-SP-DM-COPD
               WS-SP-DM-ASTHMA
               WS-SP-CASE-MGMT
               WS-SP-MATERNITY
               WS-SP-TRANSPLANT
               WS-SP-WELLNESS
               WS-SP-CHRONIC-CARE
               DELIMITED BY SIZE
               INTO WS-271-SPECIAL-PGMS
           .

      *----------------------------------------------------------------*
       3850-CHECK-WAITING-PREEXIST.
      *----------------------------------------------------------------*
      *    CHECK IF MEMBER IS STILL IN WAITING PERIOD
           IF WS-PLAN-IDX > 0
               IF WS-PLN-WAIT-PERIOD(WS-PLAN-IDX) > 0
                   COMPUTE WS-INTEGER-DATE-1 =
                       FUNCTION INTEGER-OF-DATE(HV-ME-EFF-DATE)
                   COMPUTE WS-INTEGER-DATE-2 =
                       FUNCTION INTEGER-OF-DATE(HV-SERVICE-DATE)
                   COMPUTE WS-DAYS-DIFF =
                       WS-INTEGER-DATE-2 - WS-INTEGER-DATE-1
                   IF WS-DAYS-DIFF <
                      WS-PLN-WAIT-PERIOD(WS-PLAN-IDX)
                       MOVE 'Y' TO WS-271-WAITING-PERIOD
                   ELSE
                       MOVE 'N' TO WS-271-WAITING-PERIOD
                   END-IF
               ELSE
                   MOVE 'N' TO WS-271-WAITING-PERIOD
               END-IF

      *        PRE-EXISTING CONDITION (GRANDFATHERED ONLY)
               IF WS-PLN-GRANDFATHRD(WS-PLAN-IDX) = 'Y'
                   AND WS-PLN-PREEXIST-FL(WS-PLAN-IDX) = 'Y'
                   MOVE 'Y' TO WS-271-PREEXIST-FLAG
               ELSE
                   MOVE 'N' TO WS-271-PREEXIST-FLAG
               END-IF
           END-IF
           .

      *================================================================*
      * UTILITY PARAGRAPHS
      *================================================================*

      *----------------------------------------------------------------*
       4000-VALIDATE-PLAN.
      *----------------------------------------------------------------*
           SET WS-PLAN-INVALID TO TRUE
           MOVE 0 TO WS-PLAN-IDX

           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > WS-PLAN-TABLE-COUNT
               OR WS-PLAN-IDX > 0
               IF WS-PLN-PLAN-CODE(WS-SUB-1) = HV-PLAN-CODE-IN
                   MOVE WS-SUB-1 TO WS-PLAN-IDX
                   SET WS-PLAN-VALID TO TRUE
               END-IF
           END-PERFORM

           IF WS-PLAN-INVALID
               MOVE 'EL0050' TO WS-ERR-CODE
               STRING 'PLAN CODE NOT FOUND: ' HV-PLAN-CODE-IN
                   DELIMITED BY SIZE INTO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
           END-IF
           .

      *----------------------------------------------------------------*
       4050-VALIDATE-PCP.
      *----------------------------------------------------------------*
           SET WS-PCP-INVALID TO TRUE
           MOVE E834-PCP-NPI TO HV-PCP-NPI

           EXEC SQL
               SELECT prv_npi,
                      prv_last_name || ', ' || prv_first_name,
                      prv_network_cd,
                      panel_limit, panel_current,
                      accept_new_patients,
                      prv_specialty_cd,
                      prv_zip
               INTO   :HV-PCP-NPI,
                      :HV-PCP-NAME,
                      :HV-PCP-NETWORK-CD,
                      :HV-PCP-PANEL-LIMIT,
                      :HV-PCP-PANEL-CURRENT,
                      :HV-PCP-ACCEPTING,
                      :HV-PCP-SPECIALTY,
                      :HV-PCP-ZIP
               FROM   PROVIDER_MASTER p
               LEFT JOIN T_PCP_PANEL pp
                 ON p.prv_npi = pp.pcp_npi
               WHERE  p.prv_npi = :HV-PCP-NPI
               AND    p.prv_record_status = 'A'
               AND    p.prv_pcp_flag = 'Y'
           END-EXEC

           IF SQLCODE = 0
      *        CHECK IF PCP IS IN MEMBER'S NETWORK
               IF WS-PLAN-IDX > 0
                   IF HV-PCP-NETWORK-CD =
                      WS-PLN-NETWORK-CD(WS-PLAN-IDX)
      *                CHECK IF ACCEPTING NEW PATIENTS
                       IF HV-PCP-ACCEPTING = 'Y'
      *                    CHECK PANEL CAPACITY
                           IF HV-PCP-PANEL-CURRENT <
                              HV-PCP-PANEL-LIMIT
                               SET WS-PCP-VALID TO TRUE
                               MOVE E834-PCP-NPI TO WS-PCP-NPI
                           ELSE
                               MOVE 'EL0022' TO WS-ERR-CODE
                               MOVE 'PCP PANEL IS FULL'
                                   TO WS-ERR-MSG
                               MOVE 'W' TO WS-ERR-SEV
                               PERFORM 8200-WRITE-ERROR-RECORD
                           END-IF
                       ELSE
                           MOVE 'EL0023' TO WS-ERR-CODE
                           MOVE 'PCP NOT ACCEPTING NEW PATIENTS'
                               TO WS-ERR-MSG
                           MOVE 'W' TO WS-ERR-SEV
                           PERFORM 8200-WRITE-ERROR-RECORD
                       END-IF
                   ELSE
                       MOVE 'EL0024' TO WS-ERR-CODE
                       MOVE 'PCP NOT IN MEMBER NETWORK'
                           TO WS-ERR-MSG
                       MOVE 'W' TO WS-ERR-SEV
                       PERFORM 8200-WRITE-ERROR-RECORD
                   END-IF
               END-IF
           ELSE
               MOVE 'EL0021' TO WS-ERR-CODE
               STRING 'PCP NPI NOT FOUND: ' E834-PCP-NPI
                   DELIMITED BY SIZE INTO WS-ERR-MSG
               MOVE 'W' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
           END-IF
           .

      *----------------------------------------------------------------*
       4060-AUTO-ASSIGN-PCP.
      *----------------------------------------------------------------*
      *    AUTO-ASSIGN PCP BASED ON NETWORK, GEOGRAPHY, CAPACITY
           IF WS-PLAN-IDX > 0
               EXEC SQL
                   SELECT TOP 1 p.prv_npi
                   INTO   :WS-PCP-NPI
                   FROM   PROVIDER_MASTER p
                   JOIN   T_PCP_PANEL pp
                     ON   p.prv_npi = pp.pcp_npi
                   WHERE  p.prv_network_cd =
                          :WS-PLN-NETWORK-CD(WS-PLAN-IDX)
                   AND    p.prv_pcp_flag = 'Y'
                   AND    p.prv_record_status = 'A'
                   AND    pp.accept_new_patients = 'Y'
                   AND    pp.panel_current < pp.panel_limit
                   AND    p.prv_zip = :E834-ZIP
                   ORDER BY pp.panel_current ASC
               END-EXEC

               IF SQLCODE NOT = 0
      *            NO PCP IN SAME ZIP - TRY SAME STATE
                   EXEC SQL
                       SELECT TOP 1 p.prv_npi
                       INTO   :WS-PCP-NPI
                       FROM   PROVIDER_MASTER p
                       JOIN   T_PCP_PANEL pp
                         ON   p.prv_npi = pp.pcp_npi
                       WHERE  p.prv_network_cd =
                              :WS-PLN-NETWORK-CD(WS-PLAN-IDX)
                       AND    p.prv_pcp_flag = 'Y'
                       AND    p.prv_record_status = 'A'
                       AND    pp.accept_new_patients = 'Y'
                       AND    pp.panel_current < pp.panel_limit
                       AND    p.prv_pra_state = :E834-STATE
                       ORDER BY pp.panel_current ASC
                   END-EXEC

                   IF SQLCODE NOT = 0
                       MOVE SPACES TO WS-PCP-NPI
                       MOVE 'EL0026' TO WS-ERR-CODE
                       MOVE 'NO PCP AVAILABLE FOR AUTO-ASSIGN'
                           TO WS-ERR-MSG
                       MOVE 'W' TO WS-ERR-SEV
                       PERFORM 8200-WRITE-ERROR-RECORD
                   END-IF
               END-IF

      *        INCREMENT PCP PANEL COUNT
               IF WS-PCP-NPI NOT = SPACES
                   EXEC SQL
                       UPDATE T_PCP_PANEL
                       SET    panel_current = panel_current + 1
                       WHERE  pcp_npi = :WS-PCP-NPI
                   END-EXEC
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       4070-CALCULATE-WAITING-PERIOD.
      *----------------------------------------------------------------*
      *    CALCULATE EFFECTIVE DATE AFTER WAITING PERIOD
           MOVE 'N' TO WS-WAITING-PERIOD-FL

           IF WS-PLAN-IDX > 0
               MOVE WS-PLN-WAIT-PERIOD(WS-PLAN-IDX)
                   TO WS-WAITING-PERIOD-DAYS

               IF WS-WAITING-PERIOD-DAYS > 0
      *            CALCULATE DATE AFTER WAITING PERIOD
                   MOVE E834-EFF-DATE TO WS-DATE-WORK
                   COMPUTE WS-INTEGER-DATE-1 =
                       FUNCTION INTEGER-OF-DATE(E834-EFF-DATE)
                   ADD WS-WAITING-PERIOD-DAYS TO WS-INTEGER-DATE-1
                   COMPUTE WS-DATE-NUMERIC =
                       FUNCTION DATE-OF-INTEGER(WS-INTEGER-DATE-1)
                   MOVE WS-DATE-NUMERIC TO WS-DATE-WORK

      *            FIRST-OF-MONTH-FOLLOWING LOGIC
                   IF WS-PLN-WAIT-TYPE(WS-PLAN-IDX) = 'F'
                       ADD 1 TO WS-DATE-WORK-MM
                       IF WS-DATE-WORK-MM > 12
                           MOVE 01 TO WS-DATE-WORK-MM
                           ADD 1 TO WS-DATE-WORK-YYYY
                       END-IF
                       MOVE 01 TO WS-DATE-WORK-DD
                   END-IF

                   STRING WS-DATE-WORK-YYYY WS-DATE-WORK-MM
                          WS-DATE-WORK-DD
                          DELIMITED BY SIZE
                          INTO WS-FIRST-OF-MONTH-AFTER

                   IF WS-FIRST-OF-MONTH-AFTER > WS-CURR-DATE-8
                       SET WS-IN-WAITING-PERIOD TO TRUE
                   END-IF
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       4080-CHECK-PREEXISTING.
      *----------------------------------------------------------------*
      *    PRE-EXISTING CONDITION LOGIC (GRANDFATHERED PLANS ONLY)
      *    LOOKBACK PERIOD: 6 MONTHS BEFORE ENROLLMENT
      *    EXCLUSION PERIOD: 12 MONTHS FROM ENROLLMENT
      *    REDUCED BY CREDITABLE COVERAGE
           SET WS-HAS-PREEXIST TO TRUE

      *    CHECK FOR PRIOR CREDITABLE COVERAGE
           EXEC SQL
               SELECT COUNT(*) * 30
               INTO   :WS-CREDITABLE-COV-MONTHS
               FROM   T_MEMBER_ELIG
               WHERE  member_id = :E834-MEMBER-ID
               AND    status    = 'T'
               AND    term_date >= :E834-EFF-DATE - 180
           END-EXEC

           IF WS-CREDITABLE-COV-MONTHS > 0
               COMPUTE WS-PREEXIST-EXCLUSION-MO =
                   12 - WS-CREDITABLE-COV-MONTHS
               IF WS-PREEXIST-EXCLUSION-MO <= 0
                   MOVE 'N' TO WS-PREEXIST-FLAG
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       4090-GENERATE-MEMBER-ID.
      *----------------------------------------------------------------*
      *    GENERATE NEW MEMBER ID FROM SEQUENCE TABLE
           EXEC SQL
               UPDATE T_SEQUENCE
               SET    seq_value = seq_value + 1
               WHERE  seq_name = 'MEMBER_ID'
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 870 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           EXEC SQL
               SELECT seq_value
               INTO   :HV-SEQUENCE-NO
               FROM   T_SEQUENCE
               WHERE  seq_name = 'MEMBER_ID'
           END-EXEC

           MOVE HV-SEQUENCE-NO TO WS-SEQUENCE-NO
           STRING 'MBR' WS-SEQUENCE-NO
               DELIMITED BY SIZE INTO WS-NEW-MEMBER-ID
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
           MOVE ZEROES          TO HV-MA-DED-USED-IN
           MOVE ZEROES          TO HV-MA-DED-USED-OON
           MOVE ZEROES          TO HV-MA-OOP-USED-IN
           MOVE ZEROES          TO HV-MA-OOP-USED-OON
           MOVE ZEROES          TO HV-MA-LIFETIME-USED
           MOVE ZEROES          TO HV-MA-ANNUAL-USED
           MOVE ZEROES          TO HV-MA-CARRYOVER
           MOVE WS-CURRENT-DATE-TIME TO HV-MA-LAST-UPDATED

           EXEC SQL
               INSERT INTO T_MEMBER_ACCUM
                   (member_id, plan_year,
                    ded_used_in, ded_used_oon,
                    oop_used_in, oop_used_oon,
                    lifetime_used, annual_used,
                    carryover_credit, last_updated)
               VALUES
                   (:HV-MA-MEMBER-ID, :HV-MA-PLAN-YEAR,
                    :HV-MA-DED-USED-IN, :HV-MA-DED-USED-OON,
                    :HV-MA-OOP-USED-IN, :HV-MA-OOP-USED-OON,
                    :HV-MA-LIFETIME-USED, :HV-MA-ANNUAL-USED,
                    :HV-MA-CARRYOVER, :HV-MA-LAST-UPDATED)
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-ACCUM-UPDATE-CNT
           END-IF
           .

      *----------------------------------------------------------------*
       4210-INITIALIZE-VISIT-LIMITS.
      *----------------------------------------------------------------*
           MOVE E834-MEMBER-ID TO HV-VL-MEMBER-ID
           MOVE WS-CURR-YYYY TO HV-VL-PLAN-YEAR
           MOVE ZEROES TO HV-VL-PT-USED
           MOVE ZEROES TO HV-VL-OT-USED
           MOVE ZEROES TO HV-VL-MH-USED
           MOVE ZEROES TO HV-VL-CHIRO-USED
           MOVE ZEROES TO HV-VL-SPEECH-USED
           MOVE ZEROES TO HV-VL-SUD-USED

           EXEC SQL
               INSERT INTO T_VISIT_LIMITS
                   (member_id, plan_year,
                    pt_used, ot_used, mh_used,
                    chiro_used, speech_used, sud_used)
               VALUES
                   (:HV-VL-MEMBER-ID, :HV-VL-PLAN-YEAR,
                    :HV-VL-PT-USED, :HV-VL-OT-USED,
                    :HV-VL-MH-USED, :HV-VL-CHIRO-USED,
                    :HV-VL-SPEECH-USED, :HV-VL-SUD-USED)
           END-EXEC
           .

      *----------------------------------------------------------------*
       4300-FAMILY-ACCUM-ROLLUP.
      *----------------------------------------------------------------*
      *    RECALCULATE FAMILY ACCUMULATORS BY SUMMING ALL MEMBERS
           MOVE HV-ME-SUBSCRIBER-ID TO HV-FA-SUBSCRIBER-ID
           MOVE WS-CURR-YYYY TO HV-FA-PLAN-YEAR

           EXEC SQL
               SELECT SUM(ma.ded_used_in),
                      SUM(ma.ded_used_oon),
                      SUM(ma.oop_used_in),
                      SUM(ma.oop_used_oon)
               INTO   :HV-FA-DED-USED-IN,
                      :HV-FA-DED-USED-OON,
                      :HV-FA-OOP-USED-IN,
                      :HV-FA-OOP-USED-OON
               FROM   T_MEMBER_ACCUM ma
               JOIN   T_MEMBER_ELIG me
                 ON   ma.member_id = me.member_id
               WHERE  me.subscriber_id = :HV-FA-SUBSCRIBER-ID
               AND    me.status        = 'A'
               AND    ma.plan_year     = :HV-FA-PLAN-YEAR
           END-EXEC

           IF SQLCODE = 0
      *        CAP FAMILY ACCUMULATORS AT FAMILY LIMITS
               IF WS-PLAN-IDX > 0
                   IF HV-FA-DED-USED-IN >
                      WS-PLN-FAM-DED-IN(WS-PLAN-IDX)
                       MOVE WS-PLN-FAM-DED-IN(WS-PLAN-IDX)
                           TO HV-FA-DED-USED-IN
                   END-IF
                   IF HV-FA-OOP-USED-IN >
                      WS-PLN-FAM-OOP-IN(WS-PLAN-IDX)
                       MOVE WS-PLN-FAM-OOP-IN(WS-PLAN-IDX)
                           TO HV-FA-OOP-USED-IN
                   END-IF
               END-IF

      *        CHECK EMBEDDED DEDUCTIBLE LOGIC
      *        IF 2+ MEMBERS MET INDIVIDUAL, FAMILY IS MET FOR ALL
               IF WS-PLAN-IDX > 0
                   IF WS-PLN-EMBEDDED-FL(WS-PLAN-IDX) = 'Y'
                       PERFORM 4310-CHECK-EMBEDDED-DEDUCTIBLE
                   END-IF
               END-IF

               MOVE WS-CURRENT-DATE-TIME TO HV-FA-LAST-UPDATED

               EXEC SQL
                   UPDATE T_FAMILY_ACCUM
                   SET    ded_used_in  = :HV-FA-DED-USED-IN,
                          ded_used_oon = :HV-FA-DED-USED-OON,
                          oop_used_in  = :HV-FA-OOP-USED-IN,
                          oop_used_oon = :HV-FA-OOP-USED-OON,
                          last_updated = :HV-FA-LAST-UPDATED
                   WHERE  subscriber_id = :HV-FA-SUBSCRIBER-ID
                   AND    plan_year     = :HV-FA-PLAN-YEAR
               END-EXEC

               IF SQLCODE NOT = 0
      *            INSERT IF NOT EXISTS
                   EXEC SQL
                       INSERT INTO T_FAMILY_ACCUM
                           (subscriber_id, plan_year,
                            ded_used_in, ded_used_oon,
                            oop_used_in, oop_used_oon,
                            last_updated)
                       VALUES
                           (:HV-FA-SUBSCRIBER-ID,
                            :HV-FA-PLAN-YEAR,
                            :HV-FA-DED-USED-IN,
                            :HV-FA-DED-USED-OON,
                            :HV-FA-OOP-USED-IN,
                            :HV-FA-OOP-USED-OON,
                            :HV-FA-LAST-UPDATED)
                   END-EXEC
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       4310-CHECK-EMBEDDED-DEDUCTIBLE.
      *----------------------------------------------------------------*
      *    EMBEDDED DEDUCTIBLE: WHEN N MEMBERS MEET INDIVIDUAL,
      *    FAMILY DEDUCTIBLE IS SATISFIED FOR ALL MEMBERS
           MOVE 0 TO WS-DED-MET-COUNT
           MOVE WS-PLN-EMBED-CNT(WS-PLAN-IDX)
               TO WS-EMBEDDED-THRESHOLD

           IF WS-EMBEDDED-THRESHOLD = 0
               MOVE 2 TO WS-EMBEDDED-THRESHOLD
           END-IF

      *    COUNT MEMBERS WHO HAVE MET INDIVIDUAL DEDUCTIBLE
           EXEC SQL
               SELECT COUNT(*)
               INTO   :WS-DED-MET-COUNT
               FROM   T_MEMBER_ACCUM ma
               JOIN   T_MEMBER_ELIG me
                 ON   ma.member_id = me.member_id
               JOIN   T_PLAN_BENEFIT pb
                 ON   me.plan_code = pb.plan_code
               WHERE  me.subscriber_id = :HV-FA-SUBSCRIBER-ID
               AND    me.status        = 'A'
               AND    ma.plan_year     = :HV-FA-PLAN-YEAR
               AND    ma.ded_used_in   >= pb.ind_ded_in
           END-EXEC

           IF SQLCODE = 0
               IF WS-DED-MET-COUNT >= WS-EMBEDDED-THRESHOLD
      *            FAMILY DEDUCTIBLE IS MET - UPDATE ALL MEMBERS
                   SET WS-FAM-DEDUCT-MET TO TRUE
                   MOVE WS-PLN-FAM-DED-IN(WS-PLAN-IDX)
                       TO HV-FA-DED-USED-IN
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       4500-PLAN-YEAR-RESET-CHECK.
      *----------------------------------------------------------------*
      *    CHECK IF WE HAVE CROSSED A PLAN YEAR BOUNDARY
           IF WS-PLAN-IDX = 0
               GO TO 4500-EXIT
           END-IF

           MOVE WS-PLN-YEAR-START(WS-PLAN-IDX)
               TO WS-DATE-WORK-MM
           MOVE '01' TO WS-DATE-WORK-DD
           MOVE WS-CURR-YYYY TO WS-DATE-WORK-YYYY

           STRING WS-DATE-WORK-YYYY WS-DATE-WORK-MM
                  WS-DATE-WORK-DD
                  DELIMITED BY SIZE INTO WS-PLAN-YEAR-START

      *    CALCULATE PLAN YEAR END
           MOVE WS-DATE-WORK-MM TO WS-DATE-WORK2-MM
           MOVE WS-DATE-WORK-DD TO WS-DATE-WORK2-DD
           ADD 1 TO WS-DATE-WORK-YYYY
           MOVE WS-DATE-WORK-YYYY TO WS-DATE-WORK2-YYYY

           STRING WS-DATE-WORK2-YYYY WS-DATE-WORK2-MM
                  WS-DATE-WORK2-DD
                  DELIMITED BY SIZE INTO WS-PLAN-YEAR-END

      *    CHECK IF CURRENT ACCUM YEAR EXISTS
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
      *        NO RECORD FOR CURRENT YEAR - NEEDS RESET
               SET WS-PLAN-YEAR-RESET TO TRUE

      *        CHECK FOR Q4 CARRYOVER CREDIT
               MOVE ZEROES TO WS-CARRYOVER-CREDIT
               COMPUTE WS-SUB-1 = WS-CURR-YYYY - 1
               MOVE WS-SUB-1 TO HV-MA-PLAN-YEAR

               EXEC SQL
                   SELECT ded_used_in
                   INTO   :WS-WORK-AMOUNT
                   FROM   T_MEMBER_ACCUM
                   WHERE  member_id = :HV-MA-MEMBER-ID
                   AND    plan_year = :HV-MA-PLAN-YEAR
               END-EXEC

               IF SQLCODE = 0
      *            Q4 CARRYOVER: OCT-DEC DEDUCTIBLE APPLIES TO NEXT YEAR
      *            (ONLY IF PLAN HAS CARRYOVER FEATURE)
                   IF WS-CURR-MM <= 3
                       MOVE WS-WORK-AMOUNT TO WS-CARRYOVER-CREDIT
                   END-IF
               END-IF

      *        INITIALIZE NEW YEAR ACCUMULATORS
               PERFORM 4200-INITIALIZE-ACCUMULATORS
               PERFORM 4210-INITIALIZE-VISIT-LIMITS
               ADD 1 TO WS-ACCUM-RESET-CNT

      *        IF CARRYOVER EXISTS, APPLY IT
               IF WS-CARRYOVER-CREDIT > 0
                   MOVE WS-CURR-YYYY TO HV-MA-PLAN-YEAR
                   EXEC SQL
                       UPDATE T_MEMBER_ACCUM
                       SET    carryover_credit =
                              :WS-CARRYOVER-CREDIT
                       WHERE  member_id = :HV-MA-MEMBER-ID
                       AND    plan_year = :HV-MA-PLAN-YEAR
                   END-EXEC
               END-IF
           END-IF
           .
       4500-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       4510-TRANSFER-ACCUMULATORS.
      *----------------------------------------------------------------*
      *    TRANSFER ACCUMULATORS FROM OLD PLAN TO NEW PLAN
      *    WITHIN SAME PLAN YEAR: CARRY FORWARD CREDITS
      *    DIFFERENT PLAN YEAR: RESET
           MOVE HV-ME-MEMBER-ID TO HV-MA-MEMBER-ID
           MOVE WS-CURR-YYYY TO HV-MA-PLAN-YEAR

           EXEC SQL
               SELECT ded_used_in, ded_used_oon,
                      oop_used_in, oop_used_oon,
                      lifetime_used, annual_used
               INTO   :HV-MA-DED-USED-IN, :HV-MA-DED-USED-OON,
                      :HV-MA-OOP-USED-IN, :HV-MA-OOP-USED-OON,
                      :HV-MA-LIFETIME-USED, :HV-MA-ANNUAL-USED
               FROM   T_MEMBER_ACCUM
               WHERE  member_id = :HV-MA-MEMBER-ID
               AND    plan_year = :HV-MA-PLAN-YEAR
           END-EXEC

           IF SQLCODE = 0
      *        CAP TRANSFERRED AMOUNTS AT NEW PLAN LIMITS
               IF WS-PLAN-IDX > 0
                   IF HV-MA-DED-USED-IN >
                      WS-PLN-IND-DED-IN(WS-PLAN-IDX)
                       MOVE WS-PLN-IND-DED-IN(WS-PLAN-IDX)
                           TO HV-MA-DED-USED-IN
                   END-IF
                   IF HV-MA-OOP-USED-IN >
                      WS-PLN-IND-OOP-IN(WS-PLAN-IDX)
                       MOVE WS-PLN-IND-OOP-IN(WS-PLAN-IDX)
                           TO HV-MA-OOP-USED-IN
                   END-IF
               END-IF
           END-IF
           .

      *================================================================*
      * ACA COMPLIANCE
      *================================================================*

      *----------------------------------------------------------------*
       5000-ACA-EXCHANGE-PROCESSING.
      *----------------------------------------------------------------*
           SET WS-ACA-VALID TO TRUE

      *    VALIDATE METAL TIER
           PERFORM 5100-VALIDATE-METAL-TIER

      *    VERIFY ESSENTIAL HEALTH BENEFITS COMPLIANCE
           PERFORM 5200-VERIFY-EHB

      *    VALIDATE SPECIAL ENROLLMENT PERIOD
           IF E834-SPEC-ENRL-REASON NOT = SPACES
               PERFORM 5400-VALIDATE-SEP
           END-IF

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
           IF WS-PLAN-IDX = 0
               GO TO 5100-EXIT
           END-IF

           PERFORM VARYING WS-SUB-1 FROM 1 BY 1
               UNTIL WS-SUB-1 > 5
               IF WS-METAL-CODE(WS-SUB-1) = E834-METAL-TIER
                   IF WS-PLN-AV-PCT(WS-PLAN-IDX) <
                      WS-METAL-AV-LOW(WS-SUB-1)
                      OR WS-PLN-AV-PCT(WS-PLAN-IDX) >
                      WS-METAL-AV-HIGH(WS-SUB-1)
                       MOVE 'EL0100' TO WS-ERR-CODE
                       STRING 'METAL TIER ' E834-METAL-TIER
                           ' AV MISMATCH: '
                           WS-PLN-AV-PCT(WS-PLAN-IDX)
                           DELIMITED BY SIZE INTO WS-ERR-MSG
                       MOVE 'E' TO WS-ERR-SEV
                       PERFORM 8200-WRITE-ERROR-RECORD
                       SET WS-ACA-INVALID TO TRUE
                   END-IF
               END-IF
           END-PERFORM

      *    CATASTROPHIC - AGE CHECK (UNDER 30 ONLY)
           IF E834-TIER-CATASTROPHIC
               PERFORM 4100-CALCULATE-AGE
               IF WS-AGE-YEARS >= 30
                   MOVE 'EL0101' TO WS-ERR-CODE
                   MOVE 'CATASTROPHIC PLAN: MEMBER MUST BE UNDER 30'
                       TO WS-ERR-MSG
                   MOVE 'E' TO WS-ERR-SEV
                   PERFORM 8200-WRITE-ERROR-RECORD
                   SET WS-ACA-INVALID TO TRUE
               END-IF
           END-IF
           .
       5100-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       5200-VERIFY-EHB.
      *----------------------------------------------------------------*
      *    VERIFY PLAN COVERS ALL 10 ESSENTIAL HEALTH BENEFITS
           IF WS-PLAN-IDX > 0
               IF WS-PLN-EHB-COMPL(WS-PLAN-IDX) NOT = 'Y'
                   MOVE 'EL0110' TO WS-ERR-CODE
                   MOVE 'PLAN NOT EHB COMPLIANT' TO WS-ERR-MSG
                   MOVE 'E' TO WS-ERR-SEV
                   PERFORM 8200-WRITE-ERROR-RECORD
                   SET WS-ACA-INVALID TO TRUE
               END-IF
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

      *    CHECK FOR ENHANCED APTC (AMERICAN RESCUE PLAN / IRA)
           IF WS-CURR-YYYY >= 2021
               MOVE 'Y' TO HV-AX-ENHANCED-APTC
               SET WS-ENHANCED-APTC TO TRUE
           ELSE
               MOVE 'N' TO HV-AX-ENHANCED-APTC
           END-IF

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
       5400-VALIDATE-SEP.
      *----------------------------------------------------------------*
      *    VALIDATE SPECIAL ENROLLMENT PERIOD
      *    MEMBER MUST HAVE QUALIFYING LIFE EVENT WITHIN 60 DAYS
           IF NOT (E834-SEP-MARRIAGE OR E834-SEP-BIRTH
                   OR E834-SEP-ADOPTION OR E834-SEP-LOSS-COV
                   OR E834-SEP-MOVE OR E834-SEP-COVID
                   OR E834-SEP-MEDICAID-LOSS)
               MOVE 'EL0120' TO WS-ERR-CODE
               STRING 'INVALID SEP REASON CODE: '
                   E834-SPEC-ENRL-REASON
                   DELIMITED BY SIZE INTO WS-ERR-MSG
               MOVE 'E' TO WS-ERR-SEV
               PERFORM 8200-WRITE-ERROR-RECORD
               SET WS-ACA-INVALID TO TRUE
           END-IF

      *    MEDICAID LOSS - EXTENDED 60 DAY SEP PER UNWINDING
           IF E834-SEP-MEDICAID-LOSS
               MOVE 60 TO WS-SEP-WINDOW-DAYS
           END-IF
           .

      *================================================================*
      * RETROACTIVE PROCESSING
      *================================================================*

      *----------------------------------------------------------------*
       6000-RETRO-ACCUM-RECALC.
      *----------------------------------------------------------------*
      *    RETROACTIVE ACCUMULATOR RECALCULATION
           MOVE E834-MEMBER-ID TO HV-MA-MEMBER-ID
           MOVE WS-CURR-YYYY   TO HV-MA-PLAN-YEAR

      *    ZERO OUT CURRENT ACCUMULATORS
           EXEC SQL
               UPDATE T_MEMBER_ACCUM
               SET    ded_used_in  = 0,
                      ded_used_oon = 0,
                      oop_used_in  = 0,
                      oop_used_oon = 0,
                      annual_used  = 0,
                      last_updated = :WS-CURRENT-DATE-TIME
               WHERE  member_id    = :HV-MA-MEMBER-ID
               AND    plan_year    = :HV-MA-PLAN-YEAR
           END-EXEC

      *    ZERO OUT VISIT LIMITS
           EXEC SQL
               UPDATE T_VISIT_LIMITS
               SET    pt_used = 0, ot_used = 0, mh_used = 0,
                      chiro_used = 0, speech_used = 0, sud_used = 0
               WHERE  member_id = :HV-MA-MEMBER-ID
               AND    plan_year = :HV-MA-PLAN-YEAR
           END-EXEC

      *    QUEUE RETRO FOR CLAIM REPROCESSING
           EXEC SQL
               INSERT INTO T_RETRO_QUEUE
                   (member_id, retro_eff_date, retro_term_date,
                    plan_code, queue_status, queue_type,
                    created_date, created_pgm)
               VALUES
                   (:E834-MEMBER-ID, :E834-EFF-DATE,
                    :E834-TERM-DATE, :E834-PLAN-CODE,
                    'P', 'ACCUM',
                    :WS-CURRENT-DATE-TIME, 'HCELIGVR')
           END-EXEC

      *    RECALCULATE FAMILY ACCUMULATORS
           PERFORM 4300-FAMILY-ACCUM-ROLLUP
           .

      *----------------------------------------------------------------*
       6100-FLAG-CLAIMS-REPROCESS.
      *----------------------------------------------------------------*
      *    FLAG CLAIMS FOR REPROCESSING DUE TO RETRO ENROLLMENT
           EXEC SQL
               UPDATE CLAIM_HEADER
               SET    clm_status = 'RP',
                      clm_sub_status = 'RET',
                      clm_last_upd_dt = :WS-CURR-DATE-8,
                      clm_last_upd_user = 'HCELIGVR'
               WHERE  clm_pat_mrn = :E834-MEMBER-ID
               AND    clm_from_dos >= :E834-EFF-DATE
               AND    clm_status IN ('DN', 'VD')
           END-EXEC

           IF SQLCODE = 0
               MOVE SQLERRD(3) TO WS-MEMBER-COUNT-WORK
               IF WS-MEMBER-COUNT-WORK > 0
                   DISPLAY 'HCELIGVR: FLAGGED '
                       WS-MEMBER-COUNT-WORK
                       ' CLAIMS FOR REPROCESSING - '
                       E834-MEMBER-ID
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
       6200-FLAG-CLAIMS-RECOUP.
      *----------------------------------------------------------------*
      *    FLAG PAID CLAIMS FOR RECOUPMENT DUE TO RETRO TERMINATION
           EXEC SQL
               SELECT COUNT(*), SUM(clm_net_paid_amt)
               INTO   :HV-CLAIM-COUNT, :HV-RECOUP-TOTAL
               FROM   CLAIM_HEADER
               WHERE  clm_pat_mrn   = :E834-MEMBER-ID
               AND    clm_from_dos  > :E834-TERM-DATE
               AND    clm_status    = 'PD'
           END-EXEC

           IF SQLCODE = 0 AND HV-CLAIM-COUNT > 0
               MOVE HV-CLAIM-COUNT TO WS-RC-CLAIM-COUNT
               MOVE HV-RECOUP-TOTAL TO WS-RC-TOTAL-AMOUNT

      *        WRITE RECOUPMENT RECORDS
               EXEC SQL
                   DECLARE RECOUP_CURSOR CURSOR FOR
                   SELECT clm_number, clm_from_dos,
                          clm_net_paid_amt, clm_payer_cd
                   FROM   CLAIM_HEADER
                   WHERE  clm_pat_mrn   = :E834-MEMBER-ID
                   AND    clm_from_dos  > :E834-TERM-DATE
                   AND    clm_status    = 'PD'
               END-EXEC

               EXEC SQL OPEN RECOUP_CURSOR END-EXEC

               PERFORM UNTIL SQLCODE NOT = 0
                   EXEC SQL
                       FETCH RECOUP_CURSOR
                       INTO  :RQ-CLAIM-NUMBER, :RQ-CLAIM-DOS,
                             :RQ-PAID-AMOUNT, :RQ-ORIGINAL-PLAN
                   END-EXEC

                   IF SQLCODE = 0
                       MOVE E834-MEMBER-ID TO RQ-MEMBER-ID
                       MOVE 'RTT' TO RQ-RECOUP-REASON
                       MOVE E834-PLAN-CODE TO RQ-NEW-PLAN
                       MOVE E834-EFF-DATE TO RQ-RETRO-EFF-DATE
                       MOVE E834-TERM-DATE TO RQ-RETRO-TERM-DATE
                       MOVE WS-CURR-DATE-8 TO RQ-QUEUE-DATE
                       MOVE 'P' TO RQ-QUEUE-STATUS

                       WRITE RECOUP-RECORD
                       ADD 1 TO WS-RECOUP-CNT
                   END-IF
               END-PERFORM

               EXEC SQL CLOSE RECOUP_CURSOR END-EXEC

               DISPLAY 'HCELIGVR: RECOUPMENT QUEUE - '
                   WS-RC-CLAIM-COUNT ' CLAIMS, $'
                   WS-RC-TOTAL-AMOUNT ' FOR ' E834-MEMBER-ID
           END-IF
           .

      *================================================================*
      * AUDIT TRAIL AND LOGGING
      *================================================================*

      *----------------------------------------------------------------*
       7000-WRITE-AUDIT-TRAIL.
      *----------------------------------------------------------------*
           MOVE FUNCTION CURRENT-DATE TO WS-AUD-TIMESTAMP
           MOVE WS-PROGRAM-ID        TO WS-AUD-PROGRAM
           MOVE 'BATCH'              TO WS-AUD-USER-ID
           MOVE E834-MEMBER-ID       TO WS-AUD-MEMBER-ID
           MOVE 'T_MEMBER_ELIG'      TO WS-AUD-TABLE-NAME
           MOVE E834-TRANS-TYPE       TO WS-AUD-TRANS-TYPE
           MOVE E834-PLAN-CODE        TO WS-AUD-PLAN-CODE

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

           INITIALIZE WS-AUDIT-WORK
           .

      *----------------------------------------------------------------*
       7100-LOG-ELIGIBILITY-INQUIRY.
      *----------------------------------------------------------------*
           EXEC SQL
               INSERT INTO T_ELIG_INQUIRY_LOG
                   (trans_id, member_id, service_date,
                    provider_npi, elig_status, plan_code,
                    inquiry_timestamp, response_program)
               VALUES
                   (:E270-TRANS-ID, :E270-MEMBER-ID,
                    :E270-SERVICE-DATE, :E270-PROVIDER-NPI,
                    :WS-271-ELIG-STATUS, :WS-271-PLAN-CODE,
                    :WS-CURRENT-DATE-TIME, 'HCELIGVR')
           END-EXEC
           .

      *================================================================*
      * REPORTING AND ERROR HANDLING
      *================================================================*

      *----------------------------------------------------------------*
       8000-WRITE-SUMMARY-REPORT.
      *----------------------------------------------------------------*
           PERFORM 8050-WRITE-REPORT-HEADER

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  PROCESSING SUMMARY' DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD
           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  ====================' DELIMITED BY SIZE
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
           STRING '  REACTIVATIONS:           '
               WS-REACTIVATION-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  NEWBORN ENROLLMENTS:     '
               WS-NEWBORN-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  QMCSO ORDERS:            '
               WS-QMCSO-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  DISABILITY EXTENSIONS:   '
               WS-DISABILITY-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  ACA EXCHANGE ENRL:       '
               WS-ACA-EXCHANGE-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  COBRA LETTERS:           '
               WS-COBRA-LTR-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  RECOUPMENT RECORDS:      '
               WS-RECOUP-CNT DELIMITED BY SIZE
               INTO ENRL-RPT-RECORD
           WRITE ENRL-RPT-RECORD

           MOVE SPACES TO ENRL-RPT-RECORD
           STRING '  ACCUM RESETS:            '
               WS-ACCUM-RESET-CNT DELIMITED BY SIZE
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

           IF E834-EFF-DATE NOT = SPACES AND
              E834-EFF-DATE NOT = ZEROES
               STRING E834-EFF-DATE(1:4) '-' E834-EFF-DATE(5:2)
                      '-' E834-EFF-DATE(7:2)
                   DELIMITED BY SIZE INTO WS-RPT-EFF
           END-IF

           WRITE ENRL-RPT-RECORD FROM WS-RPT-DETAIL
           ADD 1 TO WS-RPT-LINE-CNT
           INITIALIZE WS-RPT-DETAIL
           .

      *----------------------------------------------------------------*
       8200-WRITE-ERROR-RECORD.
      *----------------------------------------------------------------*
           MOVE E834-TRANS-TYPE TO ER-TRANS-TYPE
           MOVE E834-MEMBER-ID TO ER-MEMBER-ID
           MOVE WS-ERR-CODE TO ER-ERROR-CODE
           MOVE WS-ERR-MSG TO ER-ERROR-MSG
           MOVE WS-ERR-SEV TO ER-SEVERITY
           MOVE FUNCTION CURRENT-DATE TO ER-TIMESTAMP

           WRITE ERROR-RECORD

           ADD 1 TO WS-ERR-COUNT

           IF WS-ERR-IS-FATAL
               DISPLAY 'HCELIGVR: FATAL ERROR - ' WS-ERR-MSG
               MOVE 830 TO ERR-ABEND-CODE
               PERFORM 9500-ABEND-ROUTINE
           END-IF

           INITIALIZE WS-ERROR-WORK
           .

      *================================================================*
      * TERMINATION
      *================================================================*

      *----------------------------------------------------------------*
       9000-TERMINATION.
      *----------------------------------------------------------------*
           EXEC SQL COMMIT WORK END-EXEC

           EXEC SQL DISCONNECT END-EXEC

           CLOSE ENRL-834-FILE
           CLOSE ELIG-270-FILE
           CLOSE ELIG-271-FILE
           CLOSE ENRL-RPT-FILE
           CLOSE COBRA-LTR-FILE
           CLOSE ACA-RECON-OUT
           CLOSE ACCUM-RPT-FILE
           CLOSE RECOUP-FILE
           CLOSE ERROR-FILE
           CLOSE AUDIT-TRAIL-FILE

           DISPLAY '================================================='
           DISPLAY 'HCELIGVR: PROCESSING COMPLETE'
           DISPLAY '================================================='
           DISPLAY 'HCELIGVR: 834 RECORDS READ:      '
               WS-834-READ-CNT
           DISPLAY 'HCELIGVR: 834 RECORDS PROCESSED:  '
               WS-834-PROCESS-CNT
           DISPLAY 'HCELIGVR: NEW ENROLLMENTS:        '
               WS-NEW-ENROLL-CNT
           DISPLAY 'HCELIGVR: TERMINATIONS:           '
               WS-TERM-CNT
           DISPLAY 'HCELIGVR: DEPENDENT ADDS:         '
               WS-DEPEND-ADD-CNT
           DISPLAY 'HCELIGVR: PLAN CHANGES:           '
               WS-PLAN-CHNG-CNT
           DISPLAY 'HCELIGVR: COBRA ELECTIONS:         '
               WS-COBRA-CNT
           DISPLAY 'HCELIGVR: NEWBORN ENROLLMENTS:     '
               WS-NEWBORN-CNT
           DISPLAY 'HCELIGVR: RETRO CHANGES:           '
               WS-RETRO-CHG-CNT
           DISPLAY 'HCELIGVR: ACA EXCHANGE:            '
               WS-ACA-EXCHANGE-CNT
           DISPLAY 'HCELIGVR: RECOUPMENT RECORDS:      '
               WS-RECOUP-CNT
           DISPLAY 'HCELIGVR: 270 INQUIRIES:           '
               WS-270-READ-CNT
           DISPLAY 'HCELIGVR: ERRORS:                  '
               WS-834-ERROR-CNT
           DISPLAY 'HCELIGVR: AUDIT RECORDS:           '
               WS-AUDIT-WRITE-CNT
           DISPLAY '================================================='

           IF WS-834-ERROR-CNT > 0
               MOVE 4 TO WS-RETURN-CODE
           ELSE
               MOVE 0 TO WS-RETURN-CODE
           END-IF

           MOVE WS-RETURN-CODE TO RETURN-CODE
           .

      *----------------------------------------------------------------*
       9500-ABEND-ROUTINE.
      *----------------------------------------------------------------*
           DISPLAY 'HCELIGVR: ABEND INITIATED, CODE='
               ERR-ABEND-CODE

           EXEC SQL ROLLBACK WORK END-EXEC

           EXEC SQL DISCONNECT END-EXEC

           MOVE ERR-ABEND-CODE TO RETURN-CODE

           STOP RUN
           .
