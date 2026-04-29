      ****************************************************************
      * PROGRAM:    HCPRVMNT
      * AUTHOR:     ORIGINAL - J. MORRISON  1994-03-15
      * MODIFIED:   R. CHEN       1996-08-22 - ADD OIG EXCLUSION
      *             S. PATEL      1998-11-10 - Y2K REMEDIATION
      *             M. JOHNSON    2000-02-14 - NPI TRANSITION
      *             K. WILLIAMS   2001-06-30 - HIPAA COMPLIANCE
      *             T. GARCIA     2003-09-18 - SAM INTEGRATION
      *             A. THOMPSON   2005-04-22 - TIERED NETWORKS
      *             D. KUMAR      2007-11-15 - EFT PROCESSING
      *             L. ANDERSON   2009-03-01 - TAXONOMY UPDATE
      *             P. WRIGHT     2011-07-20 - ACA PROVISIONS
      *             R. MARTINEZ   2013-05-10 - ICD-10 READINESS
      *             J. TAYLOR     2015-01-15 - VALUE-BASED PAY
      *             S. LEE        2016-09-30 - MACRA/MIPS SUPPORT
      *             C. BROWN      2018-04-12 - DELEGATED CRED
      *             N. DAVIS      2019-08-25 - 1099-NEC SPLIT
      *             M. WILSON     2020-12-01 - COVID PROVISIONS
      *             A. HARRIS     2022-03-15 - NSA COMPLIANCE
      *             K. ROBINSON   2023-11-20 - PERFORMANCE TUNING
      *             T. NGUYEN     2024-06-10 - SAM V2 FORMAT
      *
      * PURPOSE:    PROVIDER MAINTENANCE AND CREDENTIALING
      *             BATCH PROCESSOR. HANDLES ALL PROVIDER
      *             LIFECYCLE EVENTS INCLUDING ENROLLMENT,
      *             CREDENTIALING, NETWORK ASSIGNMENT,
      *             PAYMENT SETUP, AND EXCLUSION MONITORING.
      *
      * INPUT:      PROVIDER TRANSACTION FILE (PRVTRANS)
      *             OIG LEIE EXCLUSION FILE (OIGEXCL)
      *             SAM EXCLUSION FILE (SAMEXCL)
      *
      * OUTPUT:     PROVIDER MASTER UPDATE (PRVMAST)
      *             CREDENTIALING REPORT (CREDRPT)
      *             NETWORK ASSIGNMENT REPORT (NETRPT)
      *             PROVIDER PAYMENT SETUP (PRVPAY)
      *             ERROR FILE (ERRFILE)
      *             AUDIT TRAIL (AUDTRL)
      *
      * FREQUENCY:  DAILY BATCH - 02:00 AM CST
      *
      * DEPENDENCIES: DB2/SYBASE PROVIDER_MASTER TABLE
      *               NPPES NPI REGISTRY EXTRACT
      *               OIG LEIE MONTHLY FILE
      *               SAM MONTHLY EXTRACT
      *
      * JCL:        JCLPRVMN (PRODUCTION)
      *             JCLPRVMT (TEST)
      *
      * ABEND CODES: U0100 - FILE OPEN ERROR
      *              U0200 - DATABASE CONNECTION FAILURE
      *              U0300 - CONTROL TOTAL MISMATCH
      *              U0400 - CRITICAL CREDENTIALING FAILURE
      *              U0500 - OIG/SAM FILE INTEGRITY ERROR
      *              U0999 - UNEXPECTED FATAL ERROR
      ****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    HCPRVMNT.
       AUTHOR.        PROVIDER-MAINTENANCE-TEAM.
       INSTALLATION.  ENTERPRISE-HEALTH-SYSTEMS.
       DATE-WRITTEN.  1994-03-15.
       DATE-COMPILED.
       SECURITY.      CONFIDENTIAL - CONTAINS PHI/PII LOGIC.
                      ACCESS RESTRICTED TO AUTHORIZED PERSONNEL.
                      HIPAA SECURITY RULE COMPLIANT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-ZOS WITH DEBUGGING MODE.
       OBJECT-COMPUTER.  IBM-ZOS.
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PROVIDER-TRANS-FILE
               ASSIGN TO PRVTRANS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRVTRANS-STATUS.

           SELECT PROVIDER-MASTER-FILE
               ASSIGN TO PRVMAST
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRVMAST-STATUS.

           SELECT CREDENTIALING-REPORT
               ASSIGN TO CREDRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CREDRPT-STATUS.

           SELECT OIG-EXCLUSION-FILE
               ASSIGN TO OIGEXCL
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OIGEXCL-STATUS.

           SELECT SAM-EXCLUSION-FILE
               ASSIGN TO SAMEXCL
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SAMEXCL-STATUS.

           SELECT NETWORK-REPORT-FILE
               ASSIGN TO NETRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-NETRPT-STATUS.

           SELECT PROVIDER-PAYMENT-FILE
               ASSIGN TO PRVPAY
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-PRVPAY-STATUS.

           SELECT ERROR-FILE
               ASSIGN TO ERRFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ERRFILE-STATUS.

           SELECT AUDIT-TRAIL-FILE
               ASSIGN TO AUDTRL
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUDTRL-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  PROVIDER-TRANS-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 1200 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS PROVIDER-TRANS-REC.

       01  PROVIDER-TRANS-REC.
           05  PTR-HEADER.
               10  PTR-RECORD-TYPE          PIC X(02).
                   88  PTR-IS-HEADER        VALUE 'HD'.
                   88  PTR-IS-DETAIL        VALUE 'DT'.
                   88  PTR-IS-TRAILER       VALUE 'TR'.
               10  PTR-TRANS-TYPE           PIC X(03).
                   88  PTR-NEW-ENROLLMENT   VALUE 'ENR'.
                   88  PTR-DEMOGRAPHIC-UPD  VALUE 'DEM'.
                   88  PTR-TERMINATION      VALUE 'TRM'.
                   88  PTR-REACTIVATION     VALUE 'REA'.
                   88  PTR-CREDENTIAL-UPD   VALUE 'CRD'.
                   88  PTR-NETWORK-CHANGE   VALUE 'NET'.
                   88  PTR-PAYMENT-SETUP    VALUE 'PAY'.
                   88  PTR-ADDRESS-CHANGE   VALUE 'ADR'.
                   88  PTR-SPECIALTY-ADD    VALUE 'SPC'.
                   88  PTR-CONTRACT-CHANGE  VALUE 'CTR'.
               10  PTR-TRANS-SEQ-NO         PIC 9(08).
               10  PTR-TRANS-DATE           PIC X(10).
               10  PTR-TRANS-TIME           PIC X(08).
               10  PTR-SOURCE-SYSTEM        PIC X(04).
               10  PTR-USER-ID              PIC X(10).
           05  PTR-PROVIDER-DATA.
               10  PTR-PROVIDER-ID          PIC X(12).
               10  PTR-NPI                  PIC X(10).
               10  PTR-ENTITY-TYPE          PIC X(01).
                   88  PTR-INDIVIDUAL       VALUE '1'.
                   88  PTR-ORGANIZATION     VALUE '2'.
               10  PTR-TAX-ID              PIC X(09).
               10  PTR-TAX-ID-TYPE         PIC X(01).
                   88  PTR-SSN             VALUE 'S'.
                   88  PTR-EIN             VALUE 'E'.
               10  PTR-LAST-NAME           PIC X(35).
               10  PTR-FIRST-NAME          PIC X(25).
               10  PTR-MIDDLE-NAME         PIC X(25).
               10  PTR-SUFFIX              PIC X(05).
               10  PTR-ORG-NAME            PIC X(60).
               10  PTR-DOB                 PIC X(10).
               10  PTR-GENDER              PIC X(01).
               10  PTR-SPECIALTY-CODE      PIC X(10).
               10  PTR-SECONDARY-SPEC      PIC X(10).
               10  PTR-TERTIARY-SPEC       PIC X(10).
               10  PTR-TAXONOMY-CODE       PIC X(10).
               10  PTR-LICENSE-NUMBER      PIC X(20).
               10  PTR-LICENSE-STATE       PIC X(02).
               10  PTR-LICENSE-EXP-DATE    PIC X(10).
               10  PTR-DEA-NUMBER          PIC X(09).
               10  PTR-DEA-EXP-DATE        PIC X(10).
               10  PTR-DEA-SCHEDULES       PIC X(05).
               10  PTR-MEDICARE-ID         PIC X(15).
               10  PTR-MEDICAID-ID         PIC X(15).
           05  PTR-ADDRESS-DATA.
               10  PTR-PRACTICE-ADDR-1     PIC X(40).
               10  PTR-PRACTICE-ADDR-2     PIC X(40).
               10  PTR-PRACTICE-CITY       PIC X(30).
               10  PTR-PRACTICE-STATE      PIC X(02).
               10  PTR-PRACTICE-ZIP        PIC X(10).
               10  PTR-PRACTICE-COUNTY     PIC X(05).
               10  PTR-PRACTICE-PHONE      PIC X(10).
               10  PTR-PRACTICE-FAX        PIC X(10).
               10  PTR-PRACTICE-EMAIL      PIC X(60).
               10  PTR-BILLING-ADDR-1      PIC X(40).
               10  PTR-BILLING-ADDR-2      PIC X(40).
               10  PTR-BILLING-CITY        PIC X(30).
               10  PTR-BILLING-STATE       PIC X(02).
               10  PTR-BILLING-ZIP         PIC X(10).
           05  PTR-CREDENTIAL-DATA.
               10  PTR-MED-SCHOOL          PIC X(60).
               10  PTR-MED-SCHOOL-GRAD-YR  PIC X(04).
               10  PTR-RESIDENCY-INST      PIC X(60).
               10  PTR-RESIDENCY-COMP-YR   PIC X(04).
               10  PTR-FELLOWSHIP-INST     PIC X(60).
               10  PTR-FELLOWSHIP-COMP-YR  PIC X(04).
               10  PTR-BOARD-CERT-STATUS   PIC X(01).
               10  PTR-BOARD-NAME          PIC X(40).
               10  PTR-BOARD-CERT-DATE     PIC X(10).
               10  PTR-BOARD-EXP-DATE      PIC X(10).
               10  PTR-MALPRACTICE-CARRIER PIC X(40).
               10  PTR-MALPRACTICE-POLICY  PIC X(20).
               10  PTR-MALPRACTICE-EXP     PIC X(10).
               10  PTR-MALPRACTICE-PER-OCC PIC 9(09).
               10  PTR-MALPRACTICE-AGGREG  PIC 9(09).
           05  PTR-PAYMENT-DATA.
               10  PTR-PAY-METHOD          PIC X(01).
                   88  PTR-PAY-EFT         VALUE 'E'.
                   88  PTR-PAY-CHECK       VALUE 'C'.
               10  PTR-BANK-ROUTING        PIC X(09).
               10  PTR-BANK-ACCOUNT        PIC X(17).
               10  PTR-BANK-ACCT-TYPE      PIC X(01).
                   88  PTR-ACCT-CHECKING   VALUE 'C'.
                   88  PTR-ACCT-SAVINGS    VALUE 'S'.
               10  PTR-W9-RECEIVED         PIC X(01).
               10  PTR-W9-DATE             PIC X(10).
               10  PTR-BACKUP-WITHHOLD     PIC X(01).
           05  PTR-TERMINATION-DATA.
               10  PTR-TERM-REASON-CODE    PIC X(03).
               10  PTR-TERM-EFF-DATE       PIC X(10).
               10  PTR-TERM-VOLUNTARY      PIC X(01).
               10  PTR-TERM-NOTIFY-MEMBERS PIC X(01).
           05  PTR-CONTRACT-DATA.
               10  PTR-CONTRACT-ID         PIC X(12).
               10  PTR-CONTRACT-EFF-DATE   PIC X(10).
               10  PTR-CONTRACT-TERM-DATE  PIC X(10).
               10  PTR-FEE-SCHED-ID        PIC X(08).
               10  PTR-NETWORK-ID          PIC X(06).
               10  PTR-TIER-LEVEL          PIC X(01).
               10  PTR-WITHHOLD-PCT        PIC 9(02)V99.
               10  PTR-QUALITY-BONUS-FLAG  PIC X(01).
               10  PTR-VBC-FLAG            PIC X(01).
               10  PTR-SHARED-SAVINGS-PCT  PIC 9(02)V99.
           05  PTR-FILLER                  PIC X(41).

       FD  PROVIDER-MASTER-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 800 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS PROVIDER-MASTER-REC.

       01  PROVIDER-MASTER-REC.
           05  PMR-PROVIDER-ID             PIC X(12).
           05  PMR-NPI                     PIC X(10).
           05  PMR-ENTITY-TYPE             PIC X(01).
           05  PMR-TAX-ID                  PIC X(09).
           05  PMR-LAST-NAME              PIC X(35).
           05  PMR-FIRST-NAME             PIC X(25).
           05  PMR-MIDDLE-NAME            PIC X(25).
           05  PMR-ORG-NAME               PIC X(60).
           05  PMR-STATUS                  PIC X(02).
           05  PMR-CRED-STATUS             PIC X(02).
           05  PMR-NETWORK-ID              PIC X(06).
           05  PMR-TIER-LEVEL              PIC X(01).
           05  PMR-SPECIALTY-CODE          PIC X(10).
           05  PMR-TAXONOMY-CODE           PIC X(10).
           05  PMR-LICENSE-STATE           PIC X(02).
           05  PMR-LICENSE-NUMBER          PIC X(20).
           05  PMR-LICENSE-EXP-DATE        PIC X(10).
           05  PMR-DEA-NUMBER              PIC X(09).
           05  PMR-CONTRACT-ID             PIC X(12).
           05  PMR-PRACTICE-ADDR-1         PIC X(40).
           05  PMR-PRACTICE-CITY           PIC X(30).
           05  PMR-PRACTICE-STATE          PIC X(02).
           05  PMR-PRACTICE-ZIP            PIC X(10).
           05  PMR-PRACTICE-COUNTY         PIC X(05).
           05  PMR-EFF-DATE                PIC X(10).
           05  PMR-TERM-DATE               PIC X(10).
           05  PMR-CRED-DATE               PIC X(10).
           05  PMR-RECRED-DUE-DATE         PIC X(10).
           05  PMR-PAY-METHOD              PIC X(01).
           05  PMR-PCP-PANEL-SIZE          PIC 9(05).
           05  PMR-PCP-PANEL-MAX           PIC 9(05).
           05  PMR-ACCEPTING-NEW           PIC X(01).
           05  PMR-OIG-CHECK-DATE          PIC X(10).
           05  PMR-SAM-CHECK-DATE          PIC X(10).
           05  PMR-CRED-SCORE              PIC 9(03).
           05  PMR-QUALITY-SCORE           PIC 9(03)V99.
           05  PMR-LAST-UPDATE-DATE        PIC X(10).
           05  PMR-LAST-UPDATE-USER        PIC X(10).
           05  PMR-ACTION-CODE             PIC X(01).
           05  PMR-FILLER                  PIC X(295).

       FD  CREDENTIALING-REPORT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS CRED-RPT-REC.

       01  CRED-RPT-REC                   PIC X(132).

       FD  OIG-EXCLUSION-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 500 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS OIG-EXCL-REC.

       01  OIG-EXCL-REC.
           05  OIG-LAST-NAME              PIC X(35).
           05  OIG-FIRST-NAME             PIC X(25).
           05  OIG-MIDDLE-NAME            PIC X(25).
           05  OIG-DOB                    PIC X(10).
           05  OIG-SSN                    PIC X(09).
           05  OIG-EIN                    PIC X(09).
           05  OIG-NPI                    PIC X(10).
           05  OIG-ENTITY-TYPE            PIC X(01).
           05  OIG-ORG-NAME               PIC X(60).
           05  OIG-ADDRESS                PIC X(40).
           05  OIG-CITY                   PIC X(30).
           05  OIG-STATE                  PIC X(02).
           05  OIG-ZIP                    PIC X(10).
           05  OIG-EXCL-TYPE              PIC X(04).
           05  OIG-EXCL-DATE              PIC X(10).
           05  OIG-REINST-DATE            PIC X(10).
           05  OIG-WAIVER-IND             PIC X(01).
           05  OIG-WAIVER-STATE           PIC X(02).
           05  OIG-SPECIALTY              PIC X(10).
           05  OIG-GENERAL-CODE           PIC X(10).
           05  OIG-FILLER                 PIC X(197).

       FD  SAM-EXCLUSION-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 600 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS SAM-EXCL-REC.

       01  SAM-EXCL-REC.
           05  SAM-CLASSIFICATION         PIC X(01).
           05  SAM-NAME                   PIC X(60).
           05  SAM-FIRST-NAME             PIC X(25).
           05  SAM-MIDDLE-NAME            PIC X(25).
           05  SAM-PREFIX                 PIC X(05).
           05  SAM-SUFFIX                 PIC X(05).
           05  SAM-DUNS                   PIC X(09).
           05  SAM-CAGE-CODE              PIC X(05).
           05  SAM-NPI                    PIC X(10).
           05  SAM-SSN-EIN               PIC X(09).
           05  SAM-ADDRESS-1             PIC X(40).
           05  SAM-ADDRESS-2             PIC X(40).
           05  SAM-CITY                  PIC X(30).
           05  SAM-STATE                 PIC X(02).
           05  SAM-ZIP                   PIC X(10).
           05  SAM-COUNTRY               PIC X(03).
           05  SAM-EXCL-PROGRAM          PIC X(04).
           05  SAM-EXCL-AGENCY           PIC X(04).
           05  SAM-CT-CODE               PIC X(02).
           05  SAM-EXCL-TYPE             PIC X(10).
           05  SAM-EXCL-DATE             PIC X(10).
           05  SAM-TERM-DATE             PIC X(10).
           05  SAM-ADDITIONAL-INFO       PIC X(100).
           05  SAM-ACTIVE-DATE           PIC X(10).
           05  SAM-RECORD-STATUS         PIC X(01).
           05  SAM-FILLER                PIC X(181).

       FD  NETWORK-REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 132 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS NET-RPT-REC.

       01  NET-RPT-REC                    PIC X(132).

       FD  PROVIDER-PAYMENT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS PRV-PAY-REC.

       01  PRV-PAY-REC.
           05  PPR-PROVIDER-ID            PIC X(12).
           05  PPR-NPI                    PIC X(10).
           05  PPR-TAX-ID                PIC X(09).
           05  PPR-PAY-METHOD            PIC X(01).
           05  PPR-BANK-ROUTING          PIC X(09).
           05  PPR-BANK-ACCOUNT          PIC X(17).
           05  PPR-BANK-ACCT-TYPE        PIC X(01).
           05  PPR-PRENOTE-STATUS        PIC X(01).
           05  PPR-PRENOTE-DATE          PIC X(10).
           05  PPR-EFT-ACTIVE-DATE       PIC X(10).
           05  PPR-W9-STATUS             PIC X(01).
           05  PPR-W9-DATE               PIC X(10).
           05  PPR-TIN-TYPE              PIC X(01).
           05  PPR-BACKUP-WITHHOLD       PIC X(01).
           05  PPR-1099-TYPE             PIC X(03).
           05  PPR-WITHHOLD-PCT          PIC 9(02)V99.
           05  PPR-YTD-PAYMENTS          PIC 9(09)V99.
           05  PPR-WITHHOLD-BALANCE      PIC 9(09)V99.
           05  PPR-EFF-DATE              PIC X(10).
           05  PPR-ACTION-CODE           PIC X(01).
           05  PPR-FILLER                PIC X(270).

       FD  ERROR-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 400 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS ERROR-REC.

       01  ERROR-REC.
           05  ERR-TIMESTAMP              PIC X(26).
           05  ERR-PROVIDER-ID            PIC X(12).
           05  ERR-NPI                    PIC X(10).
           05  ERR-TRANS-TYPE             PIC X(03).
           05  ERR-TRANS-SEQ              PIC 9(08).
           05  ERR-SEVERITY               PIC X(01).
               88  ERR-WARNING            VALUE 'W'.
               88  ERR-ERROR              VALUE 'E'.
               88  ERR-CRITICAL           VALUE 'C'.
           05  ERR-CODE                   PIC X(06).
           05  ERR-FIELD-NAME             PIC X(30).
           05  ERR-FIELD-VALUE            PIC X(60).
           05  ERR-MESSAGE                PIC X(200).
           05  ERR-PARAGRAPH-NAME         PIC X(30).
           05  ERR-FILLER                 PIC X(14).

       FD  AUDIT-TRAIL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 500 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS AUDIT-REC.

       01  AUDIT-REC.
           05  AUD-TIMESTAMP              PIC X(26).
           05  AUD-PROVIDER-ID            PIC X(12).
           05  AUD-NPI                    PIC X(10).
           05  AUD-ACTION-TYPE            PIC X(03).
           05  AUD-TRANS-SEQ              PIC 9(08).
           05  AUD-USER-ID                PIC X(10).
           05  AUD-SOURCE-SYSTEM          PIC X(04).
           05  AUD-TABLE-NAME             PIC X(30).
           05  AUD-FIELD-NAME             PIC X(30).
           05  AUD-BEFORE-VALUE           PIC X(100).
           05  AUD-AFTER-VALUE            PIC X(100).
           05  AUD-EFF-DATE               PIC X(10).
           05  AUD-RESULT-CODE            PIC X(02).
           05  AUD-RESULT-MSG             PIC X(80).
           05  AUD-FILLER                 PIC X(75).

       WORKING-STORAGE SECTION.

      ****************************************************************
      * COPYBOOKS
      ****************************************************************
           COPY CPYPROVD.
           COPY CPYSQLCA.
           COPY CPYERROR.

      ****************************************************************
      * PROGRAM CONTROL FLAGS AND FILE STATUS
      ****************************************************************
       01  WS-FILE-STATUS-FIELDS.
           05  WS-PRVTRANS-STATUS         PIC X(02).
               88  WS-PRVTRANS-OK         VALUE '00'.
               88  WS-PRVTRANS-EOF        VALUE '10'.
           05  WS-PRVMAST-STATUS          PIC X(02).
               88  WS-PRVMAST-OK          VALUE '00'.
           05  WS-CREDRPT-STATUS          PIC X(02).
               88  WS-CREDRPT-OK          VALUE '00'.
           05  WS-OIGEXCL-STATUS          PIC X(02).
               88  WS-OIGEXCL-OK          VALUE '00'.
               88  WS-OIGEXCL-EOF         VALUE '10'.
           05  WS-SAMEXCL-STATUS          PIC X(02).
               88  WS-SAMEXCL-OK          VALUE '00'.
               88  WS-SAMEXCL-EOF         VALUE '10'.
           05  WS-NETRPT-STATUS           PIC X(02).
               88  WS-NETRPT-OK           VALUE '00'.
           05  WS-PRVPAY-STATUS           PIC X(02).
               88  WS-PRVPAY-OK           VALUE '00'.
           05  WS-ERRFILE-STATUS          PIC X(02).
               88  WS-ERRFILE-OK          VALUE '00'.
           05  WS-AUDTRL-STATUS           PIC X(02).
               88  WS-AUDTRL-OK           VALUE '00'.

       01  WS-PROGRAM-FLAGS.
           05  WS-END-OF-FILE-SW          PIC X(01) VALUE 'N'.
               88  WS-END-OF-FILE         VALUE 'Y'.
               88  WS-NOT-END-OF-FILE     VALUE 'N'.
           05  WS-END-OIG-SW              PIC X(01) VALUE 'N'.
               88  WS-END-OF-OIG          VALUE 'Y'.
               88  WS-NOT-END-OIG         VALUE 'N'.
           05  WS-END-SAM-SW              PIC X(01) VALUE 'N'.
               88  WS-END-OF-SAM          VALUE 'Y'.
               88  WS-NOT-END-SAM         VALUE 'N'.
           05  WS-VALID-TRANS-SW          PIC X(01) VALUE 'Y'.
               88  WS-VALID-TRANS         VALUE 'Y'.
               88  WS-INVALID-TRANS       VALUE 'N'.
           05  WS-DB-ERROR-SW             PIC X(01) VALUE 'N'.
               88  WS-DB-ERROR            VALUE 'Y'.
               88  WS-DB-OK               VALUE 'N'.
           05  WS-FATAL-ERROR-SW          PIC X(01) VALUE 'N'.
               88  WS-FATAL-ERROR         VALUE 'Y'.
               88  WS-NO-FATAL-ERROR      VALUE 'N'.
           05  WS-PROVIDER-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-PROVIDER-FOUND      VALUE 'Y'.
               88  WS-PROVIDER-NOT-FOUND  VALUE 'N'.
           05  WS-NPI-VALID-SW            PIC X(01) VALUE 'N'.
               88  WS-NPI-VALID           VALUE 'Y'.
               88  WS-NPI-INVALID         VALUE 'N'.
           05  WS-LICENSE-VALID-SW        PIC X(01) VALUE 'N'.
               88  WS-LICENSE-VALID       VALUE 'Y'.
               88  WS-LICENSE-INVALID     VALUE 'N'.
           05  WS-DEA-VALID-SW            PIC X(01) VALUE 'N'.
               88  WS-DEA-VALID           VALUE 'Y'.
               88  WS-DEA-INVALID         VALUE 'N'.
           05  WS-TAXONOMY-VALID-SW       PIC X(01) VALUE 'N'.
               88  WS-TAXONOMY-VALID      VALUE 'Y'.
               88  WS-TAXONOMY-INVALID    VALUE 'N'.
           05  WS-OIG-MATCH-SW            PIC X(01) VALUE 'N'.
               88  WS-OIG-MATCH           VALUE 'Y'.
               88  WS-NO-OIG-MATCH        VALUE 'N'.
           05  WS-SAM-MATCH-SW            PIC X(01) VALUE 'N'.
               88  WS-SAM-MATCH           VALUE 'Y'.
               88  WS-NO-SAM-MATCH        VALUE 'N'.
           05  WS-CRED-PASS-SW            PIC X(01) VALUE 'N'.
               88  WS-CRED-PASSED         VALUE 'Y'.
               88  WS-CRED-FAILED         VALUE 'N'.
           05  WS-NETWORK-CHANGE-SW       PIC X(01) VALUE 'N'.
               88  WS-NETWORK-CHANGED     VALUE 'Y'.
               88  WS-NETWORK-UNCHANGED   VALUE 'N'.
           05  WS-BANKING-VALID-SW        PIC X(01) VALUE 'N'.
               88  WS-BANKING-VALID       VALUE 'Y'.
               88  WS-BANKING-INVALID     VALUE 'N'.
           05  WS-DELEGATED-CRED-SW       PIC X(01) VALUE 'N'.
               88  WS-IS-DELEGATED        VALUE 'Y'.
               88  WS-NOT-DELEGATED       VALUE 'N'.
           05  WS-RETRO-CHANGE-SW         PIC X(01) VALUE 'N'.
               88  WS-IS-RETROACTIVE      VALUE 'Y'.
               88  WS-NOT-RETROACTIVE     VALUE 'N'.
           05  WS-PCP-PROVIDER-SW         PIC X(01) VALUE 'N'.
               88  WS-IS-PCP              VALUE 'Y'.
               88  WS-NOT-PCP             VALUE 'N'.
           05  WS-PEDIATRIC-SW            PIC X(01) VALUE 'N'.
               88  WS-IS-PEDIATRIC        VALUE 'Y'.
               88  WS-NOT-PEDIATRIC       VALUE 'N'.

      ****************************************************************
      * COUNTERS AND ACCUMULATORS
      ****************************************************************
       01  WS-COUNTERS.
           05  WS-TRANS-READ-CTR          PIC 9(09) VALUE ZERO.
           05  WS-TRANS-PROCESSED-CTR     PIC 9(09) VALUE ZERO.
           05  WS-TRANS-ERROR-CTR         PIC 9(09) VALUE ZERO.
           05  WS-ENROLL-CTR              PIC 9(07) VALUE ZERO.
           05  WS-DEMOG-UPD-CTR           PIC 9(07) VALUE ZERO.
           05  WS-TERM-CTR                PIC 9(07) VALUE ZERO.
           05  WS-REACT-CTR               PIC 9(07) VALUE ZERO.
           05  WS-CRED-CTR                PIC 9(07) VALUE ZERO.
           05  WS-NET-CTR                 PIC 9(07) VALUE ZERO.
           05  WS-PAY-CTR                 PIC 9(07) VALUE ZERO.
           05  WS-AUDIT-CTR               PIC 9(09) VALUE ZERO.
           05  WS-ERROR-WRITTEN-CTR       PIC 9(09) VALUE ZERO.
           05  WS-MASTER-WRITTEN-CTR      PIC 9(09) VALUE ZERO.
           05  WS-OIG-RECORDS-READ        PIC 9(09) VALUE ZERO.
           05  WS-OIG-MATCHES-FOUND       PIC 9(07) VALUE ZERO.
           05  WS-OIG-EXACT-MATCHES       PIC 9(07) VALUE ZERO.
           05  WS-OIG-FUZZY-MATCHES       PIC 9(07) VALUE ZERO.
           05  WS-SAM-RECORDS-READ        PIC 9(09) VALUE ZERO.
           05  WS-SAM-MATCHES-FOUND       PIC 9(07) VALUE ZERO.
           05  WS-CRED-PASSED-CTR         PIC 9(07) VALUE ZERO.
           05  WS-CRED-FAILED-CTR         PIC 9(07) VALUE ZERO.
           05  WS-CRED-CONDITIONAL-CTR    PIC 9(07) VALUE ZERO.
           05  WS-NET-ADEQUATE-CTR        PIC 9(07) VALUE ZERO.
           05  WS-NET-GAPS-CTR            PIC 9(07) VALUE ZERO.
           05  WS-PCP-REASSIGN-CTR        PIC 9(07) VALUE ZERO.
           05  WS-EFT-SETUP-CTR           PIC 9(07) VALUE ZERO.
           05  WS-PRENOTE-CTR             PIC 9(07) VALUE ZERO.
           05  WS-1099-TRIGGER-CTR        PIC 9(07) VALUE ZERO.
           05  WS-EXCLUSION-ACTION-CTR    PIC 9(07) VALUE ZERO.
           05  WS-RECRED-DUE-CTR          PIC 9(07) VALUE ZERO.
           05  WS-DELEGATED-CRED-CTR      PIC 9(07) VALUE ZERO.
           05  WS-RETRO-CHANGE-CTR        PIC 9(07) VALUE ZERO.
           05  WS-ADDR-CHANGE-CTR         PIC 9(07) VALUE ZERO.
           05  WS-SPEC-CHANGE-CTR         PIC 9(07) VALUE ZERO.
           05  WS-CONTRACT-CHANGE-CTR     PIC 9(07) VALUE ZERO.

      ****************************************************************
      * 1099 TRACKING ACCUMULATORS
      ****************************************************************
       01  WS-1099-TRACKING.
           05  WS-1099-PROVIDER-ID        PIC X(12).
           05  WS-1099-TAX-ID             PIC X(09).
           05  WS-1099-TAX-YEAR           PIC 9(04).
           05  WS-1099-YTD-PAYMENTS       PIC 9(11)V99 VALUE ZERO.
           05  WS-1099-YTD-WITHHELD       PIC 9(11)V99 VALUE ZERO.
           05  WS-1099-THRESHOLD          PIC 9(11)V99 VALUE 600.00.
           05  WS-1099-EXCEEDS-THRESH-SW  PIC X(01) VALUE 'N'.
               88  WS-1099-EXCEEDS-THRESH VALUE 'Y'.
           05  WS-1099-TYPE-CODE          PIC X(03).
               88  WS-1099-MISC           VALUE 'MSC'.
               88  WS-1099-NEC            VALUE 'NEC'.
           05  WS-1099-BACKUP-RATE        PIC 9(02)V99 VALUE 24.00.
           05  WS-1099-CORRECTION-SW      PIC X(01) VALUE 'N'.
           05  WS-1099-B-NOTICE-SW        PIC X(01) VALUE 'N'.
           05  WS-1099-B-NOTICE-DATE      PIC X(10).
           05  WS-1099-B-NOTICE-COUNT     PIC 9(02) VALUE ZERO.

      ****************************************************************
      * DATE/TIME WORK AREAS
      ****************************************************************
       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR       PIC 9(04).
               10  WS-CURRENT-MONTH      PIC 9(02).
               10  WS-CURRENT-DAY        PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR       PIC 9(02).
               10  WS-CURRENT-MIN        PIC 9(02).
               10  WS-CURRENT-SEC        PIC 9(02).
               10  WS-CURRENT-HUND       PIC 9(02).
           05  WS-GMT-OFFSET              PIC X(05).

       01  WS-FORMATTED-TIMESTAMP         PIC X(26).
       01  WS-FORMATTED-DATE              PIC X(10).

       01  WS-DATE-WORK-AREAS.
           05  WS-DATE-WORK-1            PIC 9(08).
           05  WS-DATE-WORK-2            PIC 9(08).
           05  WS-DATE-DIFF-DAYS         PIC S9(07) VALUE ZERO.
           05  WS-DATE-DIFF-MONTHS       PIC S9(05) VALUE ZERO.
           05  WS-DATE-DIFF-YEARS        PIC S9(04) VALUE ZERO.
           05  WS-DAYS-SINCE-CRED        PIC 9(05) VALUE ZERO.
           05  WS-RECRED-CYCLE-DAYS      PIC 9(05) VALUE 1095.
           05  WS-LICENSE-DAYS-LEFT       PIC S9(05) VALUE ZERO.
           05  WS-BOARD-CERT-DAYS-LEFT   PIC S9(05) VALUE ZERO.
           05  WS-MALPRACT-DAYS-LEFT     PIC S9(05) VALUE ZERO.
           05  WS-EXPIRATION-WARNING     PIC 9(03) VALUE 90.

      ****************************************************************
      * NPI VALIDATION WORK AREAS (LUHN CHECK DIGIT)
      ****************************************************************
       01  WS-NPI-VALIDATION.
           05  WS-NPI-WORK               PIC X(10).
           05  WS-NPI-NUMERIC            PIC 9(10).
           05  WS-NPI-PREFIX             PIC 9(05) VALUE 80840.
           05  WS-NPI-DIGITS.
               10  WS-NPI-DIGIT          PIC 9(01) OCCURS 15.
           05  WS-NPI-SUM                PIC 9(05) VALUE ZERO.
           05  WS-NPI-DOUBLED            PIC 9(02) VALUE ZERO.
           05  WS-NPI-CHECK-DIGIT        PIC 9(01).
           05  WS-NPI-CALC-CHECK         PIC 9(01).
           05  WS-NPI-MOD-RESULT         PIC 9(02).
           05  WS-NPI-IDX                PIC 9(02).
           05  WS-NPI-NPPES-FOUND-SW     PIC X(01) VALUE 'N'.
               88  WS-NPI-NPPES-FOUND    VALUE 'Y'.
               88  WS-NPI-NPPES-NOTFOUND VALUE 'N'.

      ****************************************************************
      * DEA NUMBER VALIDATION WORK AREAS
      ****************************************************************
       01  WS-DEA-VALIDATION.
           05  WS-DEA-WORK               PIC X(09).
           05  WS-DEA-LETTER-1           PIC X(01).
               88  WS-DEA-PRACTITIONER   VALUE 'A' 'B' 'F'.
               88  WS-DEA-MID-LEVEL      VALUE 'M'.
               88  WS-DEA-DISTRIBUTOR    VALUE 'P' 'R'.
           05  WS-DEA-LETTER-2           PIC X(01).
           05  WS-DEA-DIGITS             PIC X(07).
           05  WS-DEA-DIGIT-ARRAY.
               10  WS-DEA-DIG            PIC 9(01) OCCURS 7.
           05  WS-DEA-SUM-ODD            PIC 9(04) VALUE ZERO.
           05  WS-DEA-SUM-EVEN           PIC 9(04) VALUE ZERO.
           05  WS-DEA-SUM-EVEN-X2        PIC 9(04) VALUE ZERO.
           05  WS-DEA-TOTAL              PIC 9(04) VALUE ZERO.
           05  WS-DEA-CHECK-DIGIT        PIC 9(01).
           05  WS-DEA-CALC-CHECK         PIC 9(01).
           05  WS-DEA-LAST-NAME-INIT     PIC X(01).
           05  WS-DEA-SCHEDULE-FLAGS.
               10  WS-DEA-SCHED-II       PIC X(01) VALUE 'N'.
               10  WS-DEA-SCHED-III      PIC X(01) VALUE 'N'.
               10  WS-DEA-SCHED-IV       PIC X(01) VALUE 'N'.
               10  WS-DEA-SCHED-V        PIC X(01) VALUE 'N'.
               10  WS-DEA-SCHED-2N       PIC X(01) VALUE 'N'.

      ****************************************************************
      * ABA ROUTING NUMBER VALIDATION WORK AREAS
      ****************************************************************
       01  WS-ABA-VALIDATION.
           05  WS-ABA-ROUTING            PIC X(09).
           05  WS-ABA-DIGIT-ARRAY.
               10  WS-ABA-DIG            PIC 9(01) OCCURS 9.
           05  WS-ABA-WEIGHTED-SUM       PIC 9(05) VALUE ZERO.
           05  WS-ABA-MOD-RESULT         PIC 9(02).
           05  WS-ABA-FED-DISTRICT       PIC 9(02).
               88  WS-ABA-VALID-DIST     VALUE 01 THRU 12.
               88  WS-ABA-THRIFT         VALUE 21 THRU 32.
               88  WS-ABA-ELECTRONIC     VALUE 61 THRU 72.

      ****************************************************************
      * PROVIDER SCORING FIELDS
      ****************************************************************
       01  WS-CREDENTIAL-SCORING.
           05  WS-CRED-EDUCATION-SCORE   PIC 9(03) VALUE ZERO.
           05  WS-CRED-LICENSE-SCORE     PIC 9(03) VALUE ZERO.
           05  WS-CRED-BOARD-SCORE       PIC 9(03) VALUE ZERO.
           05  WS-CRED-MALPRACT-SCORE    PIC 9(03) VALUE ZERO.
           05  WS-CRED-WORKHIST-SCORE    PIC 9(03) VALUE ZERO.
           05  WS-CRED-REFERENCE-SCORE   PIC 9(03) VALUE ZERO.
           05  WS-CRED-SANCTION-SCORE    PIC 9(03) VALUE ZERO.
           05  WS-CRED-SITE-VISIT-SCORE  PIC 9(03) VALUE ZERO.
           05  WS-CRED-TOTAL-SCORE       PIC 9(03) VALUE ZERO.
           05  WS-CRED-MAX-SCORE         PIC 9(03) VALUE 100.
           05  WS-CRED-PASS-THRESHOLD    PIC 9(03) VALUE 70.
           05  WS-CRED-CONDITIONAL-THRESH PIC 9(03) VALUE 55.
           05  WS-CRED-WEIGHT-EDUCATION  PIC 9(02) VALUE 20.
           05  WS-CRED-WEIGHT-LICENSE    PIC 9(02) VALUE 25.
           05  WS-CRED-WEIGHT-BOARD      PIC 9(02) VALUE 15.
           05  WS-CRED-WEIGHT-MALPRACT   PIC 9(02) VALUE 15.
           05  WS-CRED-WEIGHT-WORKHIST   PIC 9(02) VALUE 10.
           05  WS-CRED-WEIGHT-REFERENCE  PIC 9(02) VALUE 10.
           05  WS-CRED-WEIGHT-SANCTION   PIC 9(02) VALUE 05.
           05  WS-CRED-FINAL-STATUS      PIC X(02).
               88  WS-CRED-FULL-APPROVAL VALUE 'FA'.
               88  WS-CRED-CONDITIONAL   VALUE 'CA'.
               88  WS-CRED-DENIED        VALUE 'DN'.
               88  WS-CRED-PENDING       VALUE 'PD'.
               88  WS-CRED-EXPIRED       VALUE 'EX'.

      ****************************************************************
      * NETWORK TIER AND QUALITY SCORING
      ****************************************************************
       01  WS-NETWORK-SCORING.
           05  WS-NET-QUALITY-SCORE      PIC 9(03)V99 VALUE ZERO.
           05  WS-NET-COST-SCORE         PIC 9(03)V99 VALUE ZERO.
           05  WS-NET-SATISFACTION-SCORE  PIC 9(03)V99 VALUE ZERO.
           05  WS-NET-OUTCOMES-SCORE     PIC 9(03)V99 VALUE ZERO.
           05  WS-NET-COMPOSITE-SCORE    PIC 9(03)V99 VALUE ZERO.
           05  WS-NET-TIER-1-THRESHOLD   PIC 9(03)V99 VALUE 080.00.
           05  WS-NET-TIER-2-THRESHOLD   PIC 9(03)V99 VALUE 060.00.
           05  WS-NET-TIER-3-THRESHOLD   PIC 9(03)V99 VALUE 040.00.
           05  WS-NET-ASSIGNED-TIER      PIC X(01).
           05  WS-NET-PREVIOUS-TIER      PIC X(01).
           05  WS-NET-TIER-CHANGED-SW    PIC X(01) VALUE 'N'.

       01  WS-NETWORK-ADEQUACY.
           05  WS-NET-ADQ-SPECIALTY      PIC X(10).
           05  WS-NET-ADQ-COUNTY         PIC X(05).
           05  WS-NET-ADQ-URBAN-RURAL    PIC X(01).
               88  WS-NET-IS-URBAN       VALUE 'U'.
               88  WS-NET-IS-RURAL       VALUE 'R'.
           05  WS-NET-ADQ-PROV-COUNT     PIC 9(05) VALUE ZERO.
           05  WS-NET-ADQ-MBR-COUNT      PIC 9(07) VALUE ZERO.
           05  WS-NET-ADQ-RATIO          PIC 9(05)V99 VALUE ZERO.
           05  WS-NET-ADQ-URBAN-STD.
               10  WS-NET-ADQ-URB-MILES  PIC 9(03) VALUE 10.
               10  WS-NET-ADQ-URB-RATIO  PIC 9(05) VALUE 2000.
           05  WS-NET-ADQ-RURAL-STD.
               10  WS-NET-ADQ-RUR-MILES  PIC 9(03) VALUE 30.
               10  WS-NET-ADQ-RUR-RATIO  PIC 9(05) VALUE 3500.
           05  WS-NET-ADQ-MEETS-STD-SW   PIC X(01) VALUE 'N'.
               88  WS-NET-MEETS-STD      VALUE 'Y'.
               88  WS-NET-BELOW-STD      VALUE 'N'.

      ****************************************************************
      * OIG/SAM MATCH WORK AREAS
      ****************************************************************
       01  WS-OIG-MATCH-WORK.
           05  WS-OIG-MATCH-TYPE         PIC X(01).
               88  WS-OIG-EXACT-MATCH    VALUE 'E'.
               88  WS-OIG-FUZZY-MATCH    VALUE 'F'.
               88  WS-OIG-NO-MATCH       VALUE 'N'.
           05  WS-OIG-MATCH-SCORE        PIC 9(03) VALUE ZERO.
           05  WS-OIG-MATCH-THRESHOLD    PIC 9(03) VALUE 85.
           05  WS-OIG-NAME-MATCH-PCT     PIC 9(03) VALUE ZERO.
           05  WS-OIG-DOB-MATCH          PIC X(01) VALUE 'N'.
           05  WS-OIG-STATE-MATCH        PIC X(01) VALUE 'N'.
           05  WS-OIG-SSN-MATCH          PIC X(01) VALUE 'N'.
           05  WS-OIG-NPI-MATCH          PIC X(01) VALUE 'N'.
           05  WS-OIG-FALSE-POS-SW       PIC X(01) VALUE 'N'.
           05  WS-OIG-EXCLUSION-CODE     PIC X(04).
           05  WS-OIG-EXCLUSION-DESC     PIC X(60).
           05  WS-OIG-REINST-ELIGIBLE    PIC X(01) VALUE 'N'.

       01  WS-SAM-MATCH-WORK.
           05  WS-SAM-MATCH-TYPE         PIC X(01).
               88  WS-SAM-EXACT-MATCH    VALUE 'E'.
               88  WS-SAM-FUZZY-MATCH    VALUE 'F'.
               88  WS-SAM-NO-MATCH       VALUE 'N'.
           05  WS-SAM-MATCH-SCORE        PIC 9(03) VALUE ZERO.
           05  WS-SAM-MATCH-THRESHOLD    PIC 9(03) VALUE 85.
           05  WS-SAM-NAME-MATCH-PCT     PIC 9(03) VALUE ZERO.
           05  WS-SAM-NPI-MATCH          PIC X(01) VALUE 'N'.
           05  WS-SAM-TIN-MATCH          PIC X(01) VALUE 'N'.
           05  WS-SAM-FALSE-POS-SW       PIC X(01) VALUE 'N'.
           05  WS-SAM-EXCL-PROGRAM-CODE  PIC X(04).

      ****************************************************************
      * FUZZY MATCHING WORK AREAS
      ****************************************************************
       01  WS-FUZZY-MATCH-WORK.
           05  WS-FUZZY-STR-1            PIC X(60).
           05  WS-FUZZY-STR-2            PIC X(60).
           05  WS-FUZZY-LEN-1            PIC 9(02) VALUE ZERO.
           05  WS-FUZZY-LEN-2            PIC 9(02) VALUE ZERO.
           05  WS-FUZZY-MATCH-CHARS      PIC 9(02) VALUE ZERO.
           05  WS-FUZZY-TOTAL-CHARS      PIC 9(02) VALUE ZERO.
           05  WS-FUZZY-RESULT-PCT       PIC 9(03) VALUE ZERO.
           05  WS-FUZZY-IDX-1            PIC 9(02).
           05  WS-FUZZY-IDX-2            PIC 9(02).
           05  WS-FUZZY-SOUNDEX-1        PIC X(04).
           05  WS-FUZZY-SOUNDEX-2        PIC X(04).
           05  WS-FUZZY-SOUNDEX-MATCH    PIC X(01) VALUE 'N'.

      ****************************************************************
      * BEFORE/AFTER IMAGE FOR AUDIT
      ****************************************************************
       01  WS-BEFORE-IMAGE.
           05  WS-BEF-PROVIDER-ID        PIC X(12).
           05  WS-BEF-LAST-NAME          PIC X(35).
           05  WS-BEF-FIRST-NAME         PIC X(25).
           05  WS-BEF-ORG-NAME           PIC X(60).
           05  WS-BEF-ADDR-1             PIC X(40).
           05  WS-BEF-CITY               PIC X(30).
           05  WS-BEF-STATE              PIC X(02).
           05  WS-BEF-ZIP                PIC X(10).
           05  WS-BEF-COUNTY             PIC X(05).
           05  WS-BEF-PHONE              PIC X(10).
           05  WS-BEF-FAX                PIC X(10).
           05  WS-BEF-EMAIL              PIC X(60).
           05  WS-BEF-SPECIALTY          PIC X(10).
           05  WS-BEF-TAXONOMY           PIC X(10).
           05  WS-BEF-LICENSE-ST         PIC X(02).
           05  WS-BEF-LICENSE-NO         PIC X(20).
           05  WS-BEF-NETWORK-ID         PIC X(06).
           05  WS-BEF-TIER               PIC X(01).
           05  WS-BEF-STATUS             PIC X(02).
           05  WS-BEF-CONTRACT-ID        PIC X(12).
           05  WS-BEF-PAY-METHOD         PIC X(01).

      ****************************************************************
      * DATABASE HOST VARIABLES
      ****************************************************************
       01  WS-DB-PROVIDER-MASTER.
           05  HV-PROVIDER-ID            PIC X(12).
           05  HV-NPI                    PIC X(10).
           05  HV-ENTITY-TYPE            PIC X(01).
           05  HV-TAX-ID                 PIC X(09).
           05  HV-LAST-NAME              PIC X(35).
           05  HV-FIRST-NAME             PIC X(25).
           05  HV-MIDDLE-NAME            PIC X(25).
           05  HV-ORG-NAME               PIC X(60).
           05  HV-DOB                    PIC X(10).
           05  HV-GENDER                 PIC X(01).
           05  HV-STATUS                 PIC X(02).
           05  HV-CRED-STATUS            PIC X(02).
           05  HV-NETWORK-ID             PIC X(06).
           05  HV-TIER-LEVEL             PIC X(01).
           05  HV-SPECIALTY-CODE         PIC X(10).
           05  HV-SECONDARY-SPEC         PIC X(10).
           05  HV-TERTIARY-SPEC          PIC X(10).
           05  HV-TAXONOMY-CODE          PIC X(10).
           05  HV-LICENSE-STATE          PIC X(02).
           05  HV-LICENSE-NUMBER         PIC X(20).
           05  HV-LICENSE-EXP-DATE       PIC X(10).
           05  HV-DEA-NUMBER             PIC X(09).
           05  HV-DEA-EXP-DATE           PIC X(10).
           05  HV-MEDICARE-ID            PIC X(15).
           05  HV-MEDICAID-ID            PIC X(15).
           05  HV-PRACTICE-ADDR-1        PIC X(40).
           05  HV-PRACTICE-ADDR-2        PIC X(40).
           05  HV-PRACTICE-CITY          PIC X(30).
           05  HV-PRACTICE-STATE         PIC X(02).
           05  HV-PRACTICE-ZIP           PIC X(10).
           05  HV-PRACTICE-COUNTY        PIC X(05).
           05  HV-PRACTICE-PHONE         PIC X(10).
           05  HV-PRACTICE-FAX           PIC X(10).
           05  HV-PRACTICE-EMAIL         PIC X(60).
           05  HV-BILLING-ADDR-1         PIC X(40).
           05  HV-BILLING-CITY           PIC X(30).
           05  HV-BILLING-STATE          PIC X(02).
           05  HV-BILLING-ZIP            PIC X(10).
           05  HV-CONTRACT-ID            PIC X(12).
           05  HV-CONTRACT-EFF-DATE      PIC X(10).
           05  HV-CONTRACT-TERM-DATE     PIC X(10).
           05  HV-FEE-SCHED-ID           PIC X(08).
           05  HV-PAY-METHOD             PIC X(01).
           05  HV-EFT-STATUS             PIC X(01).
           05  HV-BANK-ROUTING           PIC X(09).
           05  HV-BANK-ACCOUNT           PIC X(17).
           05  HV-BANK-ACCT-TYPE         PIC X(01).
           05  HV-WITHHOLD-PCT           PIC 9(02)V99.
           05  HV-QUALITY-BONUS-FLAG     PIC X(01).
           05  HV-VBC-FLAG               PIC X(01).
           05  HV-SHARED-SAVINGS-PCT     PIC 9(02)V99.
           05  HV-PCP-PANEL-SIZE         PIC 9(05).
           05  HV-PCP-PANEL-MAX          PIC 9(05).
           05  HV-ACCEPTING-NEW          PIC X(01).
           05  HV-CRED-DATE              PIC X(10).
           05  HV-RECRED-DUE-DATE        PIC X(10).
           05  HV-CRED-SCORE             PIC 9(03).
           05  HV-QUALITY-SCORE          PIC 9(03)V99.
           05  HV-OIG-CHECK-DATE         PIC X(10).
           05  HV-SAM-CHECK-DATE         PIC X(10).
           05  HV-EFF-DATE               PIC X(10).
           05  HV-TERM-DATE              PIC X(10).
           05  HV-TERM-REASON            PIC X(03).
           05  HV-LAST-UPDATE-DATE       PIC X(10).
           05  HV-LAST-UPDATE-USER       PIC X(10).
           05  HV-LAST-UPDATE-PGM        PIC X(08).

       01  WS-DB-CREDENTIALING.
           05  HV-CRED-ID                PIC X(12).
           05  HV-CRED-PROVIDER-ID       PIC X(12).
           05  HV-CRED-TYPE              PIC X(02).
           05  HV-CRED-INIT-DATE         PIC X(10).
           05  HV-CRED-COMP-DATE         PIC X(10).
           05  HV-CRED-DECISION          PIC X(02).
           05  HV-CRED-COMMITTEE-DATE    PIC X(10).
           05  HV-CRED-ED-VERIFIED       PIC X(01).
           05  HV-CRED-LIC-VERIFIED      PIC X(01).
           05  HV-CRED-BOARD-VERIFIED    PIC X(01).
           05  HV-CRED-MAL-VERIFIED      PIC X(01).
           05  HV-CRED-WORK-VERIFIED     PIC X(01).
           05  HV-CRED-REF-VERIFIED      PIC X(01).
           05  HV-CRED-SANC-VERIFIED     PIC X(01).
           05  HV-CRED-SITE-VERIFIED     PIC X(01).
           05  HV-CRED-SCORE-VAL         PIC 9(03).
           05  HV-CRED-DELEGATED-SW      PIC X(01).
           05  HV-CRED-DELEG-ENTITY      PIC X(12).
           05  HV-CRED-DELEG-DATE        PIC X(10).
           05  HV-CRED-NPDB-QUERIED      PIC X(01).
           05  HV-CRED-NPDB-DATE         PIC X(10).
           05  HV-CRED-NPDB-RESULT       PIC X(02).

       01  WS-DB-WORK-FIELDS.
           05  HV-ROW-COUNT              PIC S9(09) COMP.
           05  HV-NEXT-PROVIDER-ID       PIC X(12).
           05  HV-NEXT-CRED-ID           PIC X(12).
           05  HV-MEMBER-COUNT           PIC S9(09) COMP.
           05  HV-CLAIMS-COUNT           PIC S9(09) COMP.
           05  HV-AUTH-COUNT             PIC S9(09) COMP.
           05  HV-PROVIDER-COUNT         PIC S9(09) COMP.
           05  HV-DELEG-AGREEMENT-STATUS PIC X(02).
           05  HV-DELEG-AGREEMENT-DATE   PIC X(10).
           05  HV-MALPRACTICE-CLAIMS     PIC S9(05) COMP.
           05  HV-YTD-TOTAL-PAID         PIC S9(11)V99 COMP-3.
           05  HV-BANK-CHANGE-COUNT      PIC S9(05) COMP.
           05  HV-BANK-CHANGE-DATE       PIC X(10).
           05  HV-PREV-TERM-DATE         PIC X(10).
           05  HV-PREV-TERM-REASON       PIC X(03).
           05  HV-NSOPW-CHECK-RESULT     PIC X(02).
           05  HV-STATE-EXCL-RESULT      PIC X(02).
           05  HV-MEDICARE-OPTOUT        PIC X(01).
           05  HV-GAP-MONTHS             PIC S9(05) COMP.

      ****************************************************************
      * TAXONOMY CODE TABLE - 200+ VALID HEALTHCARE TAXONOMY CODES
      * SOURCE: NATIONAL UNIFORM CLAIM COMMITTEE (NUCC)
      ****************************************************************
       01  WS-TAXONOMY-TABLE.
           05  WS-TAXONOMY-ENTRY OCCURS 225 TIMES
               INDEXED BY WS-TAX-IDX.
               10  WS-TAX-CODE           PIC X(10).
               10  WS-TAX-SPECIALTY      PIC X(40).
               10  WS-TAX-CATEGORY       PIC X(02).
                   88  WS-TAX-ALLOPATHIC VALUE 'AL'.
                   88  WS-TAX-OSTEOPATH  VALUE 'OS'.
                   88  WS-TAX-DENTAL     VALUE 'DN'.
                   88  WS-TAX-BEHAVIORAL VALUE 'BH'.
                   88  WS-TAX-NURSING    VALUE 'NR'.
                   88  WS-TAX-PHARMACY   VALUE 'PH'.
                   88  WS-TAX-FACILITY   VALUE 'FC'.
                   88  WS-TAX-ANCILLARY  VALUE 'AN'.
               10  WS-TAX-PCP-ELIGIBLE   PIC X(01).
               10  WS-TAX-REQUIRES-BOARD PIC X(01).
               10  WS-TAX-PEDIATRIC-IND  PIC X(01).

       01  WS-TAXONOMY-COUNT             PIC 9(03) VALUE ZERO.
       01  WS-TAXONOMY-LOADED-SW         PIC X(01) VALUE 'N'.
           88  WS-TAXONOMY-LOADED        VALUE 'Y'.

      ****************************************************************
      * TAXONOMY CODE VALUES (LOADED IN 1000-INITIALIZE)
      * ABRIDGED SET OF 225 CODES REPRESENTING ALL MAJOR CATEGORIES
      ****************************************************************
       01  WS-TAXONOMY-DATA-1.
      * ALLOPATHIC PHYSICIANS - INTERNAL MEDICINE
           05  FILLER PIC X(53) VALUE
               '207R00000XINTERNAL MEDICINE                  ALY Y N'.
           05  FILLER PIC X(53) VALUE
               '207RC0000XCARDIOVASCULAR DISEASE             ALY Y N'.
           05  FILLER PIC X(53) VALUE
               '207RE0101XENDOCRINOLOGY                      ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207RG0100XGASTROENTEROLOGY                   ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207RH0000XHEMATOLOGY                         ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207RI0001XIMMUNOLOGY                         ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207RI0200XINFECTIOUS DISEASE                 ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207RN0300XNEPHROLOGY                         ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207RP1001XPULMONARY DISEASE                  ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207RR0500XRHEUMATOLOGY                       ALN Y N'.
      * ALLOPATHIC - FAMILY MEDICINE
           05  FILLER PIC X(53) VALUE
               '207Q00000XFAMILY MEDICINE                    ALY Y N'.
           05  FILLER PIC X(53) VALUE
               '207QA0505XADULT MEDICINE                     ALY Y N'.
           05  FILLER PIC X(53) VALUE
               '207QG0300XGERIATRIC MEDICINE                 ALY Y N'.
           05  FILLER PIC X(53) VALUE
               '207QS0010XSPORTS MEDICINE                    ALN Y N'.
      * ALLOPATHIC - PEDIATRICS
           05  FILLER PIC X(53) VALUE
               '208000000XPEDIATRICS                         ALY Y Y'.
           05  FILLER PIC X(53) VALUE
               '2080A0000XPEDIATRIC ADOLESCENT MEDICINE      ALN Y Y'.
           05  FILLER PIC X(53) VALUE
               '2080C0008XPEDIATRIC CARDIOLOGY               ALN Y Y'.
           05  FILLER PIC X(53) VALUE
               '2080H0002XPEDIATRIC HEMATOLOGY-ONCOLOGY      ALN Y Y'.
           05  FILLER PIC X(53) VALUE
               '2080N0001XNEONATAL-PERINATAL MEDICINE        ALN Y Y'.
           05  FILLER PIC X(53) VALUE
               '2080P0006XPEDIATRIC DEVELOPMENTAL-BEHAVIORAL ALN Y Y'.
      * ALLOPATHIC - SURGERY
           05  FILLER PIC X(53) VALUE
               '208600000XSURGERY                            ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2086S0102XSURGICAL CRITICAL CARE             ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2086S0120XPEDIATRIC SURGERY                  ALN Y Y'.
           05  FILLER PIC X(53) VALUE
               '2086S0122XPLASTIC SURGERY WITHIN HEAD-NECK   ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2086S0105XSURGERY OF THE HAND                ALN Y N'.
      * ALLOPATHIC - ORTHOPEDIC
           05  FILLER PIC X(53) VALUE
               '207X00000XORTHOPAEDIC SURGERY                ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207XS0114XADULT RECONSTRUCTIVE ORTHOPAEDIC   ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207XS0117XORTHOPAEDIC SURGERY OF SPINE       ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207XX0004XORTHO FOOT AND ANKLE SURGERY       ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207XP3100XPEDIATRIC ORTHOPAEDIC SURGERY      ALN Y Y'.
      * ALLOPATHIC - OB/GYN
           05  FILLER PIC X(53) VALUE
               '207V00000XOBSTETRICS AND GYNECOLOGY          ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207VG0400XGYNECOLOGY                         ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207VM0101XMATERNAL AND FETAL MEDICINE         ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207VX0201XGYNECOLOGIC ONCOLOGY               ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207VE0102XREPRODUCTIVE ENDOCRINOLOGY          ALN Y N'.
      * ALLOPATHIC - PSYCHIATRY
           05  FILLER PIC X(53) VALUE
               '2084P0800XPSYCHIATRY                         BHN Y N'.
           05  FILLER PIC X(53) VALUE
               '2084P0802XADDICTION PSYCHIATRY                BHN Y N'.
           05  FILLER PIC X(53) VALUE
               '2084P0804XCHILD AND ADOLESCENT PSYCHIATRY     BHN Y Y'.
           05  FILLER PIC X(53) VALUE
               '2084P0805XGERIATRIC PSYCHIATRY                BHN Y N'.
           05  FILLER PIC X(53) VALUE
               '2084F0202XFORENSIC PSYCHIATRY                 BHN Y N'.
      * ALLOPATHIC - NEUROLOGY
           05  FILLER PIC X(53) VALUE
               '2084N0400XNEUROLOGY                          ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2084N0402XNEUROLOGY SPEC IN NEURODEVELOPMENT  ALN Y N'.
      * ALLOPATHIC - RADIOLOGY
           05  FILLER PIC X(53) VALUE
               '2085R0202XDIAGNOSTIC RADIOLOGY               ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2085R0203XTHERAPEUTIC RADIOLOGY              ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2085R0204XVASCULAR AND INTERVENTIONAL RAD    ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2085R0001XRADIATION ONCOLOGY                 ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2085D0003XDIAGNOSTIC NEUROIMAGING            ALN Y N'.
      * ALLOPATHIC - ANESTHESIOLOGY
           05  FILLER PIC X(53) VALUE
               '207L00000XANESTHESIOLOGY                     ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207LA0401XADDICTION MEDICINE                 ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207LC0200XCRITICAL CARE MEDICINE             ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207LP2900XPAIN MEDICINE                      ALN Y N'.
      * ALLOPATHIC - EMERGENCY MEDICINE
           05  FILLER PIC X(53) VALUE
               '207P00000XEMERGENCY MEDICINE                 ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207PE0004XEMERGENCY MED SPORTS MEDICINE      ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207PS0010XPEDIATRIC EMERGENCY MEDICINE       ALN Y Y'.
      * ALLOPATHIC - PATHOLOGY
           05  FILLER PIC X(53) VALUE
               '207ZP0101XANATOMIC PATHOLOGY                 ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207ZP0102XCLINICAL PATHOLOGY                 ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207ZP0104XCHEM PATHOLOGY                     ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207ZP0105XCLINICAL INFORMATICS               ALN Y N'.
      * ALLOPATHIC - DERMATOLOGY
           05  FILLER PIC X(53) VALUE
               '207N00000XDERMATOLOGY                        ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207NI0002XCLINICAL AND LAB DERMATOLOGICAL IMM ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207NS0135XDERMATOLOGIC SURGERY               ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207ND0101XDERMATOPATHOLOGY                   ALN Y N'.
      * ALLOPATHIC - OPHTHALMOLOGY
           05  FILLER PIC X(53) VALUE
               '207W00000XOPHTHALMOLOGY                      ALN Y N'.
      * ALLOPATHIC - OTOLARYNGOLOGY
           05  FILLER PIC X(53) VALUE
               '207Y00000XOTOLARYNGOLOGY                     ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207YS0123XFACIAL PLASTIC SURGERY             ALN Y N'.
      * ALLOPATHIC - UROLOGY
           05  FILLER PIC X(53) VALUE
               '208800000XUROLOGY                            ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2088P0231XPEDIATRIC UROLOGY                  ALN Y Y'.
      * ALLOPATHIC - ONCOLOGY
           05  FILLER PIC X(53) VALUE
               '207RX0202XMEDICAL ONCOLOGY                   ALN Y N'.
      * ALLOPATHIC - PHYSICAL MEDICINE
           05  FILLER PIC X(53) VALUE
               '208100000XPHYSICAL MED AND REHABILITATION    ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2081P0010XPEDIATRIC REHABILITATION MED       ALN Y Y'.
      * ALLOPATHIC - GENERAL PRACTICE
           05  FILLER PIC X(53) VALUE
               '208D00000XGENERAL PRACTICE                   ALY Y N'.
      * ALLOPATHIC - PREVENTIVE MEDICINE
           05  FILLER PIC X(53) VALUE
               '2083T0002XPREVENTIVE MEDICINE ADDICTION MED   ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2083P0500XPREVENTIVE MED AEROSPACE MEDICINE  ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2083P0901XPREVENTIVE MED PUBLIC HEALTH       ALN Y N'.
      * ALLOPATHIC - THORACIC SURGERY
           05  FILLER PIC X(53) VALUE
               '208G00000XTHORACIC SURGERY CARDIOTHORACIC    ALN Y N'.
      * ALLOPATHIC - NEUROSURGERY
           05  FILLER PIC X(53) VALUE
               '207T00000XNEUROLOGICAL SURGERY               ALN Y N'.
      * ALLOPATHIC - PLASTIC SURGERY
           05  FILLER PIC X(53) VALUE
               '208200000XPLASTIC SURGERY                    ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '2082S0105XSURGERY OF THE HAND PLASTIC        ALN Y N'.
      * ALLOPATHIC - COLON AND RECTAL SURGERY
           05  FILLER PIC X(53) VALUE
               '208C00000XCOLON AND RECTAL SURGERY           ALN Y N'.
      * ALLOPATHIC - ALLERGY AND IMMUNOLOGY
           05  FILLER PIC X(53) VALUE
               '207K00000XALLERGY AND IMMUNOLOGY             ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207KA0200XALLERGY                            ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '207KI0005XCLINICAL LAB IMMUNOLOGY            ALN Y N'.

       01  WS-TAXONOMY-DATA-2.
      * OSTEOPATHIC PHYSICIANS
           05  FILLER PIC X(53) VALUE
               '204E00000XORAL AND MAXILLOFACIAL SURGERY     OSN Y N'.
           05  FILLER PIC X(53) VALUE
               '204C00000XNEUROMUSCULOSKELETAL MED OMM       OSN Y N'.
           05  FILLER PIC X(53) VALUE
               '204D00000XNEUROMUSCULOSKELETAL MED SPORTS    OSN Y N'.
      * DENTAL
           05  FILLER PIC X(53) VALUE
               '1223G0001XGENERAL PRACTICE DENTISTRY         DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223P0221XPEDIATRIC DENTISTRY                DNN N Y'.
           05  FILLER PIC X(53) VALUE
               '1223E0200XENDODONTICS                        DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223X0400XORTHODONTICS AND DENTOFACIAL ORTHO DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223P0106XORAL AND MAXILLOFACIAL PATHOLOGY   DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223D0001XPUBLIC HEALTH DENTISTRY            DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223S0112XORAL AND MAXILLOFACIAL SURGERY DEN DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223X0008XORAL AND MAXILLOFACIAL RADIOLOGY   DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223P0700XPERIODONTICS                       DNN N N'.
           05  FILLER PIC X(53) VALUE
               '1223P0300XPROSTHODONTICS                     DNN N N'.
      * BEHAVIORAL HEALTH
           05  FILLER PIC X(53) VALUE
               '103T00000XPSYCHOLOGIST CLINICAL              BHN N N'.
           05  FILLER PIC X(53) VALUE
               '103TA0400XPSYCHOLOGIST ADDICTION              BHN N N'.
           05  FILLER PIC X(53) VALUE
               '103TC0700XPSYCHOLOGIST CLINICAL               BHN N N'.
           05  FILLER PIC X(53) VALUE
               '103TF0000XPSYCHOLOGIST FAMILY                BHN N N'.
           05  FILLER PIC X(53) VALUE
               '103TP2700XPSYCHOLOGIST PSYCHOANALYSIS        BHN N N'.
           05  FILLER PIC X(53) VALUE
               '103TP2701XPSYCHOLOGIST SCHOOL                BHN N N'.
           05  FILLER PIC X(53) VALUE
               '1041C0700XSOCIAL WORKER CLINICAL             BHN N N'.
           05  FILLER PIC X(53) VALUE
               '101Y00000XCOUNSELOR                          BHN N N'.
           05  FILLER PIC X(53) VALUE
               '101YA0400XADDICTION SUBSTANCE USE COUNSELOR  BHN N N'.
           05  FILLER PIC X(53) VALUE
               '101YM0800XMENTAL HEALTH COUNSELOR            BHN N N'.
           05  FILLER PIC X(53) VALUE
               '101YP1600XPASTORAL COUNSELOR                 BHN N N'.
           05  FILLER PIC X(53) VALUE
               '101YP2500XPROFESSIONAL COUNSELOR             BHN N N'.
           05  FILLER PIC X(53) VALUE
               '106H00000XMARRIAGE AND FAMILY THERAPIST      BHN N N'.
      * NURSING
           05  FILLER PIC X(53) VALUE
               '363L00000XNURSE PRACTITIONER                 NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363LA2100XNURSE PRACTITIONER ACUTE CARE      NRN N N'.
           05  FILLER PIC X(53) VALUE
               '363LA2200XNURSE PRACTITIONER ADULT HEALTH    NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363LC0200XNURSE PRACTITIONER CRITICAL CARE   NRN N N'.
           05  FILLER PIC X(53) VALUE
               '363LC1500XNURSE PRACTITIONER COMMUNITY HLTH  NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363LF0000XNURSE PRACTITIONER FAMILY          NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363LG0600XNURSE PRACTITIONER GERONTOLOGY     NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363LN0000XNURSE PRACTITIONER NEONATAL        NRN N Y'.
           05  FILLER PIC X(53) VALUE
               '363LN0005XNURSE PRACTITIONER NEONATAL CRIT   NRN N Y'.
           05  FILLER PIC X(53) VALUE
               '363LP0200XNURSE PRACTITIONER PEDIATRICS      NRY N Y'.
           05  FILLER PIC X(53) VALUE
               '363LP0222XNURSE PRACTITIONER PSYCH MENTAL    NRN N N'.
           05  FILLER PIC X(53) VALUE
               '363LP1700XNURSE PRACTITIONER PERINATAL       NRN N N'.
           05  FILLER PIC X(53) VALUE
               '363LP2300XNURSE PRACTITIONER PRIMARY CARE    NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363LS0200XNURSE PRACTITIONER SCHOOL          NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363LW0102XNURSE PRACTITIONER WOMENS HEALTH   NRN N N'.
           05  FILLER PIC X(53) VALUE
               '363LX0001XNURSE PRACTITIONER OBSTETRIC GYN   NRN N N'.
           05  FILLER PIC X(53) VALUE
               '364S00000XCLINICAL NURSE SPECIALIST           NRN N N'.
           05  FILLER PIC X(53) VALUE
               '367500000XNURSE ANESTHETIST CERTIFIED REG    NRN N N'.
           05  FILLER PIC X(53) VALUE
               '367A00000XADVANCED PRACTICE MIDWIFE          NRN N N'.
           05  FILLER PIC X(53) VALUE
               '363A00000XPHYSICIAN ASSISTANT                NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363AM0700XPHYSICIAN ASSISTANT MEDICAL        NRY N N'.
           05  FILLER PIC X(53) VALUE
               '363AS0400XPHYSICIAN ASSISTANT SURGICAL       NRN N N'.
      * PHARMACY
           05  FILLER PIC X(53) VALUE
               '183500000XPHARMACIST                         PHN N N'.
           05  FILLER PIC X(53) VALUE
               '1835G0000XPHARMACIST GENERAL PRACTICE        PHN N N'.
           05  FILLER PIC X(53) VALUE
               '1835N0905XPHARMACIST NUCLEAR PHARMACY        PHN N N'.
           05  FILLER PIC X(53) VALUE
               '1835P1200XPHARMACIST PHARMACOTHERAPY         PHN N N'.
           05  FILLER PIC X(53) VALUE
               '1835P1300XPHARMACIST PHARMACIST CLINICIAN    PHN N N'.
      * ANCILLARY - PHYSICAL THERAPY
           05  FILLER PIC X(53) VALUE
               '225100000XPHYSICAL THERAPIST                 ANN N N'.
           05  FILLER PIC X(53) VALUE
               '2251C2600XPHYSICAL THERAPIST CARDIOPULMONARY ANN N N'.
           05  FILLER PIC X(53) VALUE
               '2251G0304XPHYSICAL THERAPIST GERIATRICS      ANN N N'.
           05  FILLER PIC X(53) VALUE
               '2251H1300XPHYSICAL THERAPIST HAND            ANN N N'.
           05  FILLER PIC X(53) VALUE
               '2251N0400XPHYSICAL THERAPIST NEUROLOGY       ANN N N'.
           05  FILLER PIC X(53) VALUE
               '2251X0800XPHYSICAL THERAPIST ORTHOPEDIC      ANN N N'.
           05  FILLER PIC X(53) VALUE
               '2251P0200XPHYSICAL THERAPIST PEDIATRICS      ANN N Y'.
           05  FILLER PIC X(53) VALUE
               '2251S0007XPHYSICAL THERAPIST SPORTS          ANN N N'.
      * ANCILLARY - OCCUPATIONAL THERAPY
           05  FILLER PIC X(53) VALUE
               '225X00000XOCCUPATIONAL THERAPIST             ANN N N'.
           05  FILLER PIC X(53) VALUE
               '225XR0403XOCCUPATIONAL THER DRIVING REHAB    ANN N N'.
           05  FILLER PIC X(53) VALUE
               '225XH1200XOCCUPATIONAL THERAPIST HAND        ANN N N'.
           05  FILLER PIC X(53) VALUE
               '225XH1300XOCCUPATIONAL THERAPIST HUMAN FACT  ANN N N'.
           05  FILLER PIC X(53) VALUE
               '225XN1300XOCCUPATIONAL THERAPIST NEUROREHAB  ANN N N'.
           05  FILLER PIC X(53) VALUE
               '225XP0200XOCCUPATIONAL THERAPIST PEDIATRICS  ANN N Y'.
      * ANCILLARY - SPEECH LANGUAGE
           05  FILLER PIC X(53) VALUE
               '235Z00000XSPEECH-LANGUAGE PATHOLOGIST        ANN N N'.
      * ANCILLARY - AUDIOLOGY
           05  FILLER PIC X(53) VALUE
               '231H00000XAUDIOLOGIST                        ANN N N'.
           05  FILLER PIC X(53) VALUE
               '231HA2500XAUDIOLOGIST ASST TECHNOLOGY        ANN N N'.
      * ANCILLARY - OPTOMETRY
           05  FILLER PIC X(53) VALUE
               '152W00000XOPTOMETRIST                        ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '152WC0802XOPTOMETRIST CORNEAL AND CONTACT    ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '152WL0500XOPTOMETRIST LOW VISION REHABILIT   ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '152WP0200XOPTOMETRIST PEDIATRICS             ANN Y Y'.
           05  FILLER PIC X(53) VALUE
               '152WS0006XOPTOMETRIST SPORTS VISION          ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '152WX0102XOPTOMETRIST OCCUPATIONAL VISION    ANN Y N'.
      * ANCILLARY - PODIATRY
           05  FILLER PIC X(53) VALUE
               '213E00000XPODIATRIST                         ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '213EP0504XPODIATRIST PUBLIC MEDICINE         ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '213EP1101XPODIATRIST PRIMARY PODIATRIC MED   ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '213ES0131XPODIATRIST FOOT AND ANKLE SURGERY  ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '213ES0103XPODIATRIST FOOT SURGERY            ANN Y N'.
      * ANCILLARY - CHIROPRACTIC
           05  FILLER PIC X(53) VALUE
               '111N00000XCHIROPRACTOR                       ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NI0013XCHIROPRACTOR INDEPENDENT MED EXAM  ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NI0900XCHIROPRACTOR INTERNIST             ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NN0400XCHIROPRACTOR NEUROLOGY             ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NR0200XCHIROPRACTOR RADIOLOGY             ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NS0005XCHIROPRACTOR SPORTS PHYSICIAN      ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NX0100XCHIROPRACTOR OCCUPATIONAL HEALTH   ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NX0800XCHIROPRACTOR ORTHOPEDIC            ANN Y N'.
           05  FILLER PIC X(53) VALUE
               '111NP0017XCHIROPRACTOR PEDIATRIC CHIRO       ANN Y Y'.

       01  WS-TAXONOMY-DATA-3.
      * FACILITY TYPES
           05  FILLER PIC X(53) VALUE
               '282N00000XGENERAL ACUTE CARE HOSPITAL        FCN N N'.
           05  FILLER PIC X(53) VALUE
               '282NC0060XCRITICAL ACCESS HOSPITAL           FCN N N'.
           05  FILLER PIC X(53) VALUE
               '282NC2000XCHILDRENS HOSPITAL                 FCN N Y'.
           05  FILLER PIC X(53) VALUE
               '282NR1301XRURAL ACUTE CARE HOSPITAL          FCN N N'.
           05  FILLER PIC X(53) VALUE
               '282NW0100XWOMENS HOSPITAL                    FCN N N'.
           05  FILLER PIC X(53) VALUE
               '283Q00000XPSYCHIATRIC HOSPITAL               FCN N N'.
           05  FILLER PIC X(53) VALUE
               '283X00000XREHABILITATION HOSPITAL            FCN N N'.
           05  FILLER PIC X(53) VALUE
               '283XC2000XCHILDRENS REHABILITATION HOSPITAL  FCN N Y'.
           05  FILLER PIC X(53) VALUE
               '284300000XSPECIAL HOSPITAL                   FCN N N'.
           05  FILLER PIC X(53) VALUE
               '281P00000XCHRONIC DISEASE HOSPITAL           FCN N N'.
           05  FILLER PIC X(53) VALUE
               '281PC2000XCHILDRENS CHRONIC DISEASE HOSPITAL FCN N Y'.
           05  FILLER PIC X(53) VALUE
               '261QM1300XAMBULATORY SURGICAL CENTER         FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QA1903XAMBULATORY CARE CLINIC             FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QC1500XCOMMUNITY HEALTH CENTER            FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QD0000XCLINIC CENTER AMBULATORY DERM      FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QE0002XEMERGENCY CARE CLINIC              FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QF0400XFEDERALLY QUALIFIED HEALTH CTR     FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QH0100XHOME HEALTH AGENCY                 FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QM0801XMENTAL HEALTH CENTER               FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QM1000XMIGRANT HEALTH CENTER              FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QP2300XPRIMARY CARE CLINIC                FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QR0200XRADIOLOGY CLINIC                   FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QR0206XMAMMOGRAPHY CLINIC                 FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QR0400XREHABILITATION OUTPATIENT          FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QR0401XREHABILITATION SUBSTANCE USE       FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QR0800XRECOVERY CARE CLINIC               FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QR1100XRESEARCH CLINIC                    FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QR1300XRURAL HEALTH CLINIC                FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QS0112XORAL AND MAXILLOFACIAL SURG CLINIC FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QS1000XSTUDENT HEALTH CLINIC              FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QS1200XSURGERY CLINIC                     FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QU0200XURGENT CARE CLINIC                 FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QV0200XVA CLINIC                          FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QX0200XONCOLOGY CLINIC                    FCN N N'.
           05  FILLER PIC X(53) VALUE
               '261QX0203XRADIATION ONCOLOGY CLINIC          FCN N N'.
      * SKILLED NURSING / LONG TERM CARE
           05  FILLER PIC X(53) VALUE
               '314000000XSKILLED NURSING FACILITY           FCN N N'.
           05  FILLER PIC X(53) VALUE
               '3140N1450XNURSING CARE SNF PEDIATRIC         FCN N Y'.
           05  FILLER PIC X(53) VALUE
               '311500000XALZHEIMERS CENTER DEMENTIA CTR     FCN N N'.
           05  FILLER PIC X(53) VALUE
               '311Z00000XCUSTODIAL CARE FACILITY            FCN N N'.
           05  FILLER PIC X(53) VALUE
               '313M00000XNURSING FACILITY INTERMEDIATE CARE FCN N N'.
           05  FILLER PIC X(53) VALUE
               '310400000XASSISTED LIVING FACILITY           FCN N N'.
      * LABORATORY
           05  FILLER PIC X(53) VALUE
               '291U00000XCLINICAL MEDICAL LABORATORY        ANN N N'.
           05  FILLER PIC X(53) VALUE
               '293D00000XPHYSIOLOGICAL LABORATORY           ANN N N'.
      * DME / SUPPLIES
           05  FILLER PIC X(53) VALUE
               '332B00000XDURABLE MEDICAL EQUIPMENT SUPPLIER ANN N N'.
           05  FILLER PIC X(53) VALUE
               '332BC3200XCUSTOMIZED EQUIPMENT DME           ANN N N'.
           05  FILLER PIC X(53) VALUE
               '332BD1200XDIALYSIS EQUIPMENT DME             ANN N N'.
           05  FILLER PIC X(53) VALUE
               '332BN1400XNURSING FACILITY DME SUPPLIES      ANN N N'.
           05  FILLER PIC X(53) VALUE
               '332BP3500XPARENTERAL AND ENTERAL NUTRITION   ANN N N'.
           05  FILLER PIC X(53) VALUE
               '332BX2000XOXYGEN EQUIPMENT SUPPLIES          ANN N N'.
      * TRANSPORTATION
           05  FILLER PIC X(53) VALUE
               '341600000XAMBULANCE AIR TRANSPORT            ANN N N'.
           05  FILLER PIC X(53) VALUE
               '341800000XAMBULANCE GROUND TRANSPORT         ANN N N'.
           05  FILLER PIC X(53) VALUE
               '343900000XNON-EMERGENCY MEDICAL TRANSPORT    ANN N N'.
      * ADDITIONAL SPECIALTIES
           05  FILLER PIC X(53) VALUE
               '174400000XSPECIALIST                         ALN Y N'.
           05  FILLER PIC X(53) VALUE
               '174H00000XHEALTH EDUCATOR                    ANN N N'.
           05  FILLER PIC X(53) VALUE
               '174M00000XNUTRITIONIST                       ANN N N'.
           05  FILLER PIC X(53) VALUE
               '175L00000XHOMEOPATH                          ANN N N'.
           05  FILLER PIC X(53) VALUE
               '175M00000XMIDWIFE LAY                        ANN N N'.
           05  FILLER PIC X(53) VALUE
               '176B00000XMIDWIFE                            ANN N N'.
           05  FILLER PIC X(53) VALUE
               '176P00000XFUNEREAL DIRECTOR                  ANN N N'.
           05  FILLER PIC X(53) VALUE
               '177F00000XLODGING PROVIDER                   ANN N N'.
           05  FILLER PIC X(53) VALUE
               '246ZB0301XBLOOD BANK SPECIALIST              ANN N N'.
           05  FILLER PIC X(53) VALUE
               '246ZC0007XCYTOTECHNOLOGIST                   ANN N N'.
      * FILLER ENTRIES TO REACH 225 TOTAL
           05  FILLER PIC X(53) VALUE
               '246ZN0300XNUCLEAR MEDICINE TECHNOLOGIST      ANN N N'.
           05  FILLER PIC X(53) VALUE
               '247100000XRADIOLOGIC TECHNOLOGIST            ANN N N'.
           05  FILLER PIC X(53) VALUE
               '247200000XOTHER TECHNICIAN                   ANN N N'.
           05  FILLER PIC X(53) VALUE
               '251B00000XCASE MANAGER NURSE                 NRN N N'.
           05  FILLER PIC X(53) VALUE
               '251E00000XHOME HEALTH AIDE                   ANN N N'.
           05  FILLER PIC X(53) VALUE
               '251K00000XHOME HEALTH AIDE CERTIFIED         ANN N N'.
           05  FILLER PIC X(53) VALUE
               '252Y00000XEARLY INTERVENTION PROVIDER AGENCY ANN N N'.
           05  FILLER PIC X(53) VALUE
               '253Z00000XMULTISPECIALTY PRACTICE             FCN N N'.
           05  FILLER PIC X(53) VALUE
               '390200000XSTUDENT IN ORGANIZED HEALTH CARE   ANN N N'.

       01  WS-TAX-TABLE-REDEFINE.
           05  WS-TAX-DATA-ALL.
               10  WS-TAX-RAW-1          PIC X(3975).
               10  WS-TAX-RAW-2          PIC X(4346).
               10  WS-TAX-RAW-3          PIC X(3604).

      ****************************************************************
      * STATE LICENSING BOARD TABLE
      ****************************************************************
       01  WS-STATE-LICENSE-TABLE.
           05  WS-STATE-ENTRY OCCURS 55 TIMES
               INDEXED BY WS-ST-IDX.
               10  WS-ST-CODE            PIC X(02).
               10  WS-ST-BOARD-NAME      PIC X(40).
               10  WS-ST-COMPACT-MEMBER  PIC X(01).
               10  WS-ST-RENEWAL-CYCLE   PIC 9(02).
               10  WS-ST-CME-REQUIRED    PIC 9(03).
               10  WS-ST-DEA-REQUIRED    PIC X(01).
               10  WS-ST-CSR-REQUIRED    PIC X(01).

       01  WS-STATE-COUNT                PIC 9(02) VALUE ZERO.

      ****************************************************************
      * CREDENTIAL VERIFICATION STATUS TRACKING
      ****************************************************************
       01  WS-CRED-VERIFICATION-STATUS.
           05  WS-CRED-VERIFY-EDUCATION.
               10  WS-CRED-V-ED-STATUS   PIC X(02).
                   88  WS-CRED-V-ED-PASS VALUE 'PS'.
                   88  WS-CRED-V-ED-FAIL VALUE 'FL'.
                   88  WS-CRED-V-ED-PEND VALUE 'PD'.
                   88  WS-CRED-V-ED-NA   VALUE 'NA'.
               10  WS-CRED-V-ED-DATE     PIC X(10).
               10  WS-CRED-V-ED-SOURCE   PIC X(30).
               10  WS-CRED-V-ED-NOTES    PIC X(80).
           05  WS-CRED-VERIFY-LICENSE.
               10  WS-CRED-V-LIC-STATUS  PIC X(02).
                   88  WS-CRED-V-LIC-PASS VALUE 'PS'.
                   88  WS-CRED-V-LIC-FAIL VALUE 'FL'.
                   88  WS-CRED-V-LIC-PEND VALUE 'PD'.
               10  WS-CRED-V-LIC-DATE    PIC X(10).
               10  WS-CRED-V-LIC-SOURCE  PIC X(30).
               10  WS-CRED-V-LIC-NOTES   PIC X(80).
               10  WS-CRED-V-LIC-ACTION  PIC X(02).
                   88  WS-LIC-CLEAR       VALUE 'CL'.
                   88  WS-LIC-PROBATION   VALUE 'PR'.
                   88  WS-LIC-SUSPENDED   VALUE 'SU'.
                   88  WS-LIC-REVOKED     VALUE 'RV'.
                   88  WS-LIC-RESTRICTED  VALUE 'RS'.
                   88  WS-LIC-SURRENDERED VALUE 'SR'.
           05  WS-CRED-VERIFY-BOARD.
               10  WS-CRED-V-BRD-STATUS  PIC X(02).
                   88  WS-CRED-V-BRD-PASS VALUE 'PS'.
                   88  WS-CRED-V-BRD-FAIL VALUE 'FL'.
                   88  WS-CRED-V-BRD-PEND VALUE 'PD'.
                   88  WS-CRED-V-BRD-NA   VALUE 'NA'.
               10  WS-CRED-V-BRD-DATE    PIC X(10).
               10  WS-CRED-V-BRD-SOURCE  PIC X(30).
               10  WS-CRED-V-BRD-EXP     PIC X(10).
           05  WS-CRED-VERIFY-MALPRACT.
               10  WS-CRED-V-MAL-STATUS  PIC X(02).
                   88  WS-CRED-V-MAL-PASS VALUE 'PS'.
                   88  WS-CRED-V-MAL-FAIL VALUE 'FL'.
                   88  WS-CRED-V-MAL-PEND VALUE 'PD'.
               10  WS-CRED-V-MAL-DATE    PIC X(10).
               10  WS-CRED-V-MAL-PER-OCC PIC 9(09).
               10  WS-CRED-V-MAL-AGGREG  PIC 9(09).
               10  WS-CRED-V-MAL-CLAIMS  PIC 9(03).
               10  WS-CRED-V-MAL-NPDB    PIC X(02).
           05  WS-CRED-VERIFY-WORKHIST.
               10  WS-CRED-V-WRK-STATUS  PIC X(02).
                   88  WS-CRED-V-WRK-PASS VALUE 'PS'.
                   88  WS-CRED-V-WRK-FAIL VALUE 'FL'.
                   88  WS-CRED-V-WRK-PEND VALUE 'PD'.
               10  WS-CRED-V-WRK-DATE    PIC X(10).
               10  WS-CRED-V-WRK-GAPS    PIC 9(02).
               10  WS-CRED-V-WRK-EXPLAIN PIC X(01).
           05  WS-CRED-VERIFY-REFERENCES.
               10  WS-CRED-V-REF-STATUS  PIC X(02).
                   88  WS-CRED-V-REF-PASS VALUE 'PS'.
                   88  WS-CRED-V-REF-FAIL VALUE 'FL'.
                   88  WS-CRED-V-REF-PEND VALUE 'PD'.
               10  WS-CRED-V-REF-DATE    PIC X(10).
               10  WS-CRED-V-REF-COUNT   PIC 9(02).
           05  WS-CRED-VERIFY-SANCTIONS.
               10  WS-CRED-V-SAN-STATUS  PIC X(02).
                   88  WS-CRED-V-SAN-PASS VALUE 'PS'.
                   88  WS-CRED-V-SAN-FAIL VALUE 'FL'.
                   88  WS-CRED-V-SAN-PEND VALUE 'PD'.
               10  WS-CRED-V-SAN-DATE    PIC X(10).
               10  WS-CRED-V-SAN-OIG     PIC X(01).
               10  WS-CRED-V-SAN-SAM     PIC X(01).
               10  WS-CRED-V-SAN-STATE   PIC X(01).
               10  WS-CRED-V-SAN-NSOPW   PIC X(01).
               10  WS-CRED-V-SAN-DEA     PIC X(01).
               10  WS-CRED-V-SAN-MCRE-OO PIC X(01).
           05  WS-CRED-VERIFY-SITE.
               10  WS-CRED-V-SITE-STATUS PIC X(02).
                   88  WS-CRED-V-SITE-PASS VALUE 'PS'.
                   88  WS-CRED-V-SITE-FAIL VALUE 'FL'.
                   88  WS-CRED-V-SITE-PEND VALUE 'PD'.
                   88  WS-CRED-V-SITE-NA   VALUE 'NA'.
               10  WS-CRED-V-SITE-DATE   PIC X(10).
               10  WS-CRED-V-SITE-SCORE  PIC 9(03).

      ****************************************************************
      * CONTRACT RATE TABLES
      ****************************************************************
       01  WS-CONTRACT-RATE-TABLE.
           05  WS-RATE-ENTRY OCCURS 50 TIMES
               INDEXED BY WS-RATE-IDX.
               10  WS-RATE-FEE-SCHED-ID  PIC X(08).
               10  WS-RATE-NETWORK-ID    PIC X(06).
               10  WS-RATE-TIER          PIC X(01).
               10  WS-RATE-EFF-DATE      PIC X(10).
               10  WS-RATE-TERM-DATE     PIC X(10).
               10  WS-RATE-BASE-PCT      PIC 9(03)V99.
               10  WS-RATE-ESCALATION    PIC 9(01)V99.
               10  WS-RATE-WITHHOLD-PCT  PIC 9(02)V99.
               10  WS-RATE-QUALITY-BONUS PIC 9(02)V99.
               10  WS-RATE-VBC-TARGET    PIC 9(09)V99.
               10  WS-RATE-SHARED-SAV    PIC 9(02)V99.
               10  WS-RATE-RISK-UPPER    PIC 9(03)V99.
               10  WS-RATE-RISK-LOWER    PIC 9(03)V99.

      ****************************************************************
      * NETWORK TIER TABLES
      ****************************************************************
       01  WS-NETWORK-TIER-TABLE.
           05  WS-TIER-ENTRY OCCURS 20 TIMES
               INDEXED BY WS-TIER-IDX.
               10  WS-TIER-NETWORK-ID    PIC X(06).
               10  WS-TIER-CODE          PIC X(01).
               10  WS-TIER-NAME          PIC X(20).
               10  WS-TIER-QUAL-MIN      PIC 9(03)V99.
               10  WS-TIER-COST-MIN      PIC 9(03)V99.
               10  WS-TIER-SAT-MIN       PIC 9(03)V99.
               10  WS-TIER-OUTCOME-MIN   PIC 9(03)V99.
               10  WS-TIER-COMPOSITE-MIN PIC 9(03)V99.

      ****************************************************************
      * ZIP-TO-COUNTY MAPPING FOR GEOCODING
      ****************************************************************
       01  WS-ZIP-COUNTY-WORK.
           05  WS-ZIP-INPUT              PIC X(05).
           05  WS-ZIP-COUNTY-CODE        PIC X(05).
           05  WS-ZIP-COUNTY-NAME        PIC X(30).
           05  WS-ZIP-STATE              PIC X(02).
           05  WS-ZIP-URBAN-RURAL        PIC X(01).
           05  WS-ZIP-LATITUDE           PIC S9(03)V9(06).
           05  WS-ZIP-LONGITUDE          PIC S9(03)V9(06).
           05  WS-ZIP-MSA-CODE           PIC X(05).

      ****************************************************************
      * TERMINATION REASON CODE TABLE
      ****************************************************************
       01  WS-TERM-REASON-TABLE.
           05  WS-TERM-ENTRY OCCURS 35 TIMES
               INDEXED BY WS-TERM-IDX.
               10  WS-TERM-CODE          PIC X(03).
               10  WS-TERM-DESC          PIC X(50).
               10  WS-TERM-VOLUNTARY-IND PIC X(01).
               10  WS-TERM-NOTIFY-REQ    PIC X(01).
               10  WS-TERM-RECOUP-REQ    PIC X(01).
               10  WS-TERM-REPORT-REQ    PIC X(01).

       01  WS-TERM-REASON-DATA.
           05  FILLER PIC X(56) VALUE
               'R01VOLUNTARY RETIREMENT                            YNNN'.
           05  FILLER PIC X(56) VALUE
               'R02VOLUNTARY RELOCATION OUT OF SERVICE AREA        YYNN'.
           05  FILLER PIC X(56) VALUE
               'R03VOLUNTARY PRACTICE CLOSURE                      YYNN'.
           05  FILLER PIC X(56) VALUE
               'R04VOLUNTARY JOINING ANOTHER NETWORK               YYNN'.
           05  FILLER PIC X(56) VALUE
               'R05VOLUNTARY CHANGE IN PRACTICE SCOPE              YYNN'.
           05  FILLER PIC X(56) VALUE
               'R06VOLUNTARY CONTRACT NON-RENEWAL BY PROVIDER      YYYN'.
           05  FILLER PIC X(56) VALUE
               'R07VOLUNTARY HEALTH DISABILITY                     YYNN'.
           05  FILLER PIC X(56) VALUE
               'R08DEATH OF PROVIDER                               YYNN'.
           05  FILLER PIC X(56) VALUE
               'I01INVOLUNTARY LOSS OF STATE LICENSE                NYYN'.
           05  FILLER PIC X(56) VALUE
               'I02INVOLUNTARY LICENSE SUSPENSION                   NYYY'.
           05  FILLER PIC X(56) VALUE
               'I03INVOLUNTARY LICENSE REVOCATION                   NYYY'.
           05  FILLER PIC X(56) VALUE
               'I04INVOLUNTARY LICENSE PROBATION W RESTRICTION     NYYY'.
           05  FILLER PIC X(56) VALUE
               'I05INVOLUNTARY DEA REGISTRATION LOSS               NYYY'.
           05  FILLER PIC X(56) VALUE
               'I06INVOLUNTARY LOSS OF BOARD CERTIFICATION         NYYN'.
           05  FILLER PIC X(56) VALUE
               'I07INVOLUNTARY FAILURE TO MAINTAIN MALPRACTICE     NYYY'.
           05  FILLER PIC X(56) VALUE
               'I08INVOLUNTARY FAILURE TO COMPLETE RECREDENTIALING NYYY'.
           05  FILLER PIC X(56) VALUE
               'I09INVOLUNTARY OIG EXCLUSION LIST MATCH            NYYY'.
           05  FILLER PIC X(56) VALUE
               'I10INVOLUNTARY SAM EXCLUSION LIST MATCH            NYYY'.
           05  FILLER PIC X(56) VALUE
               'I11INVOLUNTARY STATE MEDICAID EXCLUSION            NYYY'.
           05  FILLER PIC X(56) VALUE
               'I12INVOLUNTARY FRAUD OR ABUSE FINDING              NYYY'.
           05  FILLER PIC X(56) VALUE
               'I13INVOLUNTARY QUALITY OF CARE CONCERN             NYYY'.
           05  FILLER PIC X(56) VALUE
               'I14INVOLUNTARY PATIENT SAFETY EVENT                NYYY'.
           05  FILLER PIC X(56) VALUE
               'I15INVOLUNTARY NON-COMPLIANCE WITH CONTRACT TERMS  NYYY'.
           05  FILLER PIC X(56) VALUE
               'I16INVOLUNTARY FAILURE TO MEET ACCESS STANDARDS    NYYY'.
           05  FILLER PIC X(56) VALUE
               'I17INVOLUNTARY CRIMINAL CONVICTION                 NYYY'.
           05  FILLER PIC X(56) VALUE
               'I18INVOLUNTARY NPDB ADVERSE ACTION REPORT          NYYY'.
           05  FILLER PIC X(56) VALUE
               'I19INVOLUNTARY MEDICARE OPTOUT NON-PARTICIPATING   NYYY'.
           05  FILLER PIC X(56) VALUE
               'I20INVOLUNTARY LOSS OF HOSPITAL PRIVILEGES         NYYY'.
           05  FILLER PIC X(56) VALUE
               'I21INVOLUNTARY FALSIFICATION OF CREDENTIALS        NYYY'.
           05  FILLER PIC X(56) VALUE
               'I22INVOLUNTARY SEXUAL MISCONDUCT                   NYYY'.
           05  FILLER PIC X(56) VALUE
               'I23INVOLUNTARY IMPAIRMENT SUBSTANCE ABUSE          NYYY'.
           05  FILLER PIC X(56) VALUE
               'N01NETWORK CONTRACT NON-RENEWAL BY PLAN            NYNN'.
           05  FILLER PIC X(56) VALUE
               'N02NETWORK REDUCTION FOR ADEQUACY ADJUSTMENT       NYNN'.
           05  FILLER PIC X(56) VALUE
               'N03NETWORK MERGER OR CONSOLIDATION                 NYNN'.
           05  FILLER PIC X(56) VALUE
               'N04NETWORK PRODUCT LINE DISCONTINUATION            NYNN'.

      ****************************************************************
      * REPORT LINE FORMATTING
      ****************************************************************
       01  WS-RPT-HEADER-1.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  FILLER                    PIC X(30) VALUE
               'ENTERPRISE HEALTH SYSTEMS'.
           05  FILLER                    PIC X(40) VALUE SPACES.
           05  WS-RPT-TITLE             PIC X(40).
           05  FILLER                    PIC X(06) VALUE 'DATE: '.
           05  WS-RPT-DATE              PIC X(10).
           05  FILLER                    PIC X(05) VALUE SPACES.

       01  WS-RPT-HEADER-2.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  FILLER                    PIC X(30) VALUE
               'PROVIDER MAINTENANCE SYSTEM'.
           05  FILLER                    PIC X(40) VALUE SPACES.
           05  WS-RPT-SUBTITLE          PIC X(40).
           05  FILLER                    PIC X(06) VALUE 'PAGE: '.
           05  WS-RPT-PAGE-NO           PIC ZZ,ZZ9.
           05  FILLER                    PIC X(09) VALUE SPACES.

       01  WS-RPT-DETAIL-LINE           PIC X(132).

       01  WS-CRED-RPT-DETAIL.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-PROVIDER-ID         PIC X(12).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-NPI                 PIC X(10).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-PROVIDER-NAME       PIC X(30).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-SPECIALTY           PIC X(15).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-CRED-STATUS         PIC X(12).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-CRED-SCORE          PIC ZZ9.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-CRED-DATE           PIC X(10).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-RECRED-DUE          PIC X(10).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-CR-ALERT               PIC X(10).
           05  FILLER                    PIC X(01) VALUE SPACES.

       01  WS-NET-RPT-DETAIL.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-NETWORK-ID          PIC X(06).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-SPECIALTY           PIC X(15).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-COUNTY              PIC X(20).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-PROV-COUNT          PIC ZZ,ZZ9.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-MBR-COUNT           PIC ZZZ,ZZ9.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-RATIO               PIC ZZ,ZZ9.99.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-STANDARD            PIC ZZ,ZZ9.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-MEETS-STD           PIC X(03).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-URBAN-RURAL         PIC X(05).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-NR-GAP-IND             PIC X(03).
           05  FILLER                    PIC X(24) VALUE SPACES.

       01  WS-SANC-RPT-DETAIL.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-PROVIDER-ID         PIC X(12).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-NPI                 PIC X(10).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-PROVIDER-NAME       PIC X(30).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-MATCH-SOURCE        PIC X(05).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-MATCH-TYPE          PIC X(06).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-MATCH-SCORE         PIC ZZ9.
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-EXCL-CODE           PIC X(04).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-EXCL-DATE           PIC X(10).
           05  FILLER                    PIC X(01) VALUE SPACES.
           05  WS-SR-ACTION-TAKEN        PIC X(20).
           05  FILLER                    PIC X(15) VALUE SPACES.

       01  WS-RPT-PAGE-COUNTERS.
           05  WS-CRED-RPT-PAGE          PIC 9(05) VALUE ZERO.
           05  WS-CRED-RPT-LINE          PIC 9(02) VALUE 99.
           05  WS-NET-RPT-PAGE           PIC 9(05) VALUE ZERO.
           05  WS-NET-RPT-LINE           PIC 9(02) VALUE 99.
           05  WS-SANC-RPT-PAGE          PIC 9(05) VALUE ZERO.
           05  WS-SANC-RPT-LINE          PIC 9(02) VALUE 99.
           05  WS-LINES-PER-PAGE         PIC 9(02) VALUE 55.

      ****************************************************************
      * MISCELLANEOUS WORK AREAS
      ****************************************************************
       01  WS-WORK-AREAS.
           05  WS-WORK-STRING-1          PIC X(100).
           05  WS-WORK-STRING-2          PIC X(100).
           05  WS-WORK-NUMBER-1          PIC 9(09) VALUE ZERO.
           05  WS-WORK-NUMBER-2          PIC 9(09) VALUE ZERO.
           05  WS-WORK-INDEX             PIC 9(04) VALUE ZERO.
           05  WS-WORK-REMAINDER         PIC 9(04) VALUE ZERO.
           05  WS-WORK-QUOTIENT          PIC 9(09) VALUE ZERO.
           05  WS-WORK-LENGTH            PIC 9(02) VALUE ZERO.
           05  WS-WORK-CHAR              PIC X(01).
           05  WS-RETURN-CODE            PIC S9(04) VALUE ZERO.
           05  WS-ABEND-CODE             PIC X(04).
           05  WS-PROGRAM-NAME           PIC X(08) VALUE 'HCPRVMNT'.
           05  WS-DB-SUBSYSTEM           PIC X(04) VALUE 'DSN1'.
           05  WS-SQLCODE-DISPLAY        PIC -(09).
           05  WS-PROVIDER-SEQ-CTR       PIC 9(12) VALUE ZERO.
           05  WS-CRED-SEQ-CTR           PIC 9(12) VALUE ZERO.
           05  WS-PREV-BANK-ROUTING      PIC X(09).
           05  WS-PREV-BANK-ACCOUNT      PIC X(17).
           05  WS-DISTANCE-MILES         PIC 9(05)V99.
           05  WS-PANEL-AVAILABLE        PIC 9(05).
           05  WS-TAX-ID-WORK            PIC X(09).
           05  WS-TAX-ID-SUM             PIC 9(05).

      ****************************************************************
      * SQLCA FOR EMBEDDED SQL
      ****************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      ****************************************************************
      * PROCEDURE DIVISION
      ****************************************************************
       PROCEDURE DIVISION.

      ****************************************************************
      * 0000-MAIN-CONTROL
      * MAIN DRIVER FOR PROVIDER MAINTENANCE BATCH PROCESSING.
      * CONTROLS THE OVERALL FLOW: INITIALIZE, PROCESS TRANSACTIONS,
      * RUN OIG/SAM EXCLUSION CHECKS, GENERATE REPORTS, TERMINATE.
      ****************************************************************
       0000-MAIN-CONTROL.

           PERFORM 1000-INITIALIZE

           IF WS-NO-FATAL-ERROR
               PERFORM 1500-PROCESS-TRANSACTIONS
                   UNTIL WS-END-OF-FILE OR WS-FATAL-ERROR

               IF WS-NO-FATAL-ERROR
                   PERFORM 6000-PROCESS-OIG-FILE
                   PERFORM 6200-PROCESS-SAM-FILE
                   PERFORM 7000-GENERATE-CREDENTIALING-REPORT
                   PERFORM 7100-GENERATE-NETWORK-REPORT
                   PERFORM 7200-GENERATE-SANCTION-REPORT
               END-IF
           END-IF

           PERFORM 9000-TERMINATION

           STOP RUN.

      ****************************************************************
      * 1000-INITIALIZE
      * OPENS ALL FILES, CONNECTS TO DATABASE, LOADS REFERENCE
      * TABLES (TAXONOMY CODES, STATE LICENSING BOARDS, TERMINATION
      * REASON CODES, CONTRACT RATES, NETWORK TIERS), VALIDATES
      * CONTROL RECORDS, INITIALIZES ALL COUNTERS AND WORK AREAS.
      ****************************************************************
       1000-INITIALIZE.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA

           STRING WS-CURRENT-YEAR '-'
                  WS-CURRENT-MONTH '-'
                  WS-CURRENT-DAY
                  DELIMITED BY SIZE INTO WS-FORMATTED-DATE
           END-STRING

           STRING WS-CURRENT-YEAR '-'
                  WS-CURRENT-MONTH '-'
                  WS-CURRENT-DAY ' '
                  WS-CURRENT-HOUR ':'
                  WS-CURRENT-MIN ':'
                  WS-CURRENT-SEC '.'
                  WS-CURRENT-HUND '0000'
                  DELIMITED BY SIZE INTO WS-FORMATTED-TIMESTAMP
           END-STRING

           DISPLAY 'HCPRVMNT - PROVIDER MAINTENANCE BATCH STARTING'
           DISPLAY 'HCPRVMNT - RUN DATE: ' WS-FORMATTED-DATE
           DISPLAY 'HCPRVMNT - RUN TIME: ' WS-CURRENT-HOUR ':'
                   WS-CURRENT-MIN ':' WS-CURRENT-SEC

      * OPEN ALL INPUT AND OUTPUT FILES
           OPEN INPUT PROVIDER-TRANS-FILE
           IF NOT WS-PRVTRANS-OK
               DISPLAY 'HCPRVMNT - FATAL: CANNOT OPEN PRVTRANS '
                       'FILE STATUS=' WS-PRVTRANS-STATUS
               MOVE 'Y' TO WS-FATAL-ERROR-SW
               MOVE 'U0100' TO WS-ABEND-CODE
               PERFORM 9000-TERMINATION
               STOP RUN
           END-IF

           OPEN INPUT OIG-EXCLUSION-FILE
           IF NOT WS-OIGEXCL-OK
               DISPLAY 'HCPRVMNT - FATAL: CANNOT OPEN OIGEXCL '
                       'FILE STATUS=' WS-OIGEXCL-STATUS
               MOVE 'Y' TO WS-FATAL-ERROR-SW
               MOVE 'U0100' TO WS-ABEND-CODE
               PERFORM 9000-TERMINATION
               STOP RUN
           END-IF

           OPEN INPUT SAM-EXCLUSION-FILE
           IF NOT WS-SAMEXCL-OK
               DISPLAY 'HCPRVMNT - FATAL: CANNOT OPEN SAMEXCL '
                       'FILE STATUS=' WS-SAMEXCL-STATUS
               MOVE 'Y' TO WS-FATAL-ERROR-SW
               MOVE 'U0100' TO WS-ABEND-CODE
               PERFORM 9000-TERMINATION
               STOP RUN
           END-IF

           OPEN OUTPUT PROVIDER-MASTER-FILE
           IF NOT WS-PRVMAST-OK
               DISPLAY 'HCPRVMNT - FATAL: CANNOT OPEN PRVMAST '
                       'FILE STATUS=' WS-PRVMAST-STATUS
               MOVE 'Y' TO WS-FATAL-ERROR-SW
               MOVE 'U0100' TO WS-ABEND-CODE
               PERFORM 9000-TERMINATION
               STOP RUN
           END-IF

           OPEN OUTPUT CREDENTIALING-REPORT
           IF NOT WS-CREDRPT-OK
               DISPLAY 'HCPRVMNT - WARNING: CANNOT OPEN CREDRPT '
                       'FILE STATUS=' WS-CREDRPT-STATUS
           END-IF

           OPEN OUTPUT NETWORK-REPORT-FILE
           IF NOT WS-NETRPT-OK
               DISPLAY 'HCPRVMNT - WARNING: CANNOT OPEN NETRPT '
                       'FILE STATUS=' WS-NETRPT-STATUS
           END-IF

           OPEN OUTPUT PROVIDER-PAYMENT-FILE
           IF NOT WS-PRVPAY-OK
               DISPLAY 'HCPRVMNT - WARNING: CANNOT OPEN PRVPAY '
                       'FILE STATUS=' WS-PRVPAY-STATUS
           END-IF

           OPEN OUTPUT ERROR-FILE
           IF NOT WS-ERRFILE-OK
               DISPLAY 'HCPRVMNT - WARNING: CANNOT OPEN ERRFILE '
                       'FILE STATUS=' WS-ERRFILE-STATUS
           END-IF

           OPEN OUTPUT AUDIT-TRAIL-FILE
           IF NOT WS-AUDTRL-OK
               DISPLAY 'HCPRVMNT - WARNING: CANNOT OPEN AUDTRL '
                       'FILE STATUS=' WS-AUDTRL-STATUS
           END-IF

      * CONNECT TO SYBASE DATABASE
           EXEC SQL
               CONNECT TO :WS-DB-SUBSYSTEM
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCPRVMNT - FATAL: DATABASE CONNECT FAILED'
               DISPLAY 'HCPRVMNT - SQLCODE=' SQLCODE
               MOVE 'Y' TO WS-FATAL-ERROR-SW
               MOVE 'U0200' TO WS-ABEND-CODE
               PERFORM 9000-TERMINATION
               STOP RUN
           END-IF

           DISPLAY 'HCPRVMNT - DATABASE CONNECTION ESTABLISHED'

      * LOAD TAXONOMY CODE TABLE FROM WORKING-STORAGE DATA
           PERFORM 1010-LOAD-TAXONOMY-TABLE
      * LOAD STATE LICENSING BOARD TABLE FROM DATABASE
           PERFORM 1020-LOAD-STATE-LICENSE-TABLE
      * LOAD TERMINATION REASON TABLE FROM WORKING-STORAGE DATA
           PERFORM 1030-LOAD-TERM-REASON-TABLE
      * LOAD CONTRACT RATE TABLE FROM DATABASE
           PERFORM 1040-LOAD-CONTRACT-RATE-TABLE
      * LOAD NETWORK TIER TABLE FROM DATABASE
           PERFORM 1050-LOAD-NETWORK-TIER-TABLE

      * READ FIRST TRANSACTION
           READ PROVIDER-TRANS-FILE
               AT END
                   SET WS-END-OF-FILE TO TRUE
                   DISPLAY 'HCPRVMNT - WARNING: EMPTY INPUT FILE'
               NOT AT END
                   ADD 1 TO WS-TRANS-READ-CTR
      *            VALIDATE HEADER RECORD
                   IF PTR-IS-HEADER
                       DISPLAY 'HCPRVMNT - HEADER RECORD READ'
                       DISPLAY 'HCPRVMNT - SOURCE: '
                               PTR-SOURCE-SYSTEM
                       READ PROVIDER-TRANS-FILE
                           AT END
                               SET WS-END-OF-FILE TO TRUE
                           NOT AT END
                               ADD 1 TO WS-TRANS-READ-CTR
                       END-READ
                   END-IF
           END-READ

           DISPLAY 'HCPRVMNT - INITIALIZATION COMPLETE'.

      ****************************************************************
      * 1010-LOAD-TAXONOMY-TABLE
      * LOADS 225 HEALTHCARE TAXONOMY CODES FROM WORKING-STORAGE
      * DATA AREAS INTO THE SEARCHABLE TAXONOMY TABLE. EACH ENTRY
      * CONTAINS THE NUCC TAXONOMY CODE, SPECIALTY DESCRIPTION,
      * CATEGORY, PCP ELIGIBILITY, AND BOARD CERT REQUIREMENT.
      ****************************************************************
       1010-LOAD-TAXONOMY-TABLE.

           INITIALIZE WS-TAXONOMY-TABLE

           MOVE 1 TO WS-WORK-INDEX

      *    LOAD FROM DATA-1 BLOCK (75 ENTRIES)
           PERFORM VARYING WS-TAX-IDX FROM 1 BY 1
               UNTIL WS-TAX-IDX > 75
               MOVE WS-TAX-RAW-1(WS-WORK-INDEX:10)
                   TO WS-TAX-CODE(WS-TAX-IDX)
               MOVE WS-TAX-RAW-1(WS-WORK-INDEX + 10:40)
                   TO WS-TAX-SPECIALTY(WS-TAX-IDX)
               MOVE WS-TAX-RAW-1(WS-WORK-INDEX + 50:2)
                   TO WS-TAX-CATEGORY(WS-TAX-IDX)
               MOVE WS-TAX-RAW-1(WS-WORK-INDEX + 52:1)
                   TO WS-TAX-PCP-ELIGIBLE(WS-TAX-IDX)
               ADD 53 TO WS-WORK-INDEX
           END-PERFORM

      *    CONTINUE FROM DATA-2 BLOCK
           MOVE 1 TO WS-WORK-INDEX
           PERFORM VARYING WS-TAX-IDX FROM 76 BY 1
               UNTIL WS-TAX-IDX > 157
               MOVE WS-TAX-RAW-2(WS-WORK-INDEX:10)
                   TO WS-TAX-CODE(WS-TAX-IDX)
               MOVE WS-TAX-RAW-2(WS-WORK-INDEX + 10:40)
                   TO WS-TAX-SPECIALTY(WS-TAX-IDX)
               MOVE WS-TAX-RAW-2(WS-WORK-INDEX + 50:2)
                   TO WS-TAX-CATEGORY(WS-TAX-IDX)
               MOVE WS-TAX-RAW-2(WS-WORK-INDEX + 52:1)
                   TO WS-TAX-PCP-ELIGIBLE(WS-TAX-IDX)
               ADD 53 TO WS-WORK-INDEX
           END-PERFORM

      *    CONTINUE FROM DATA-3 BLOCK
           MOVE 1 TO WS-WORK-INDEX
           PERFORM VARYING WS-TAX-IDX FROM 158 BY 1
               UNTIL WS-TAX-IDX > 225
               MOVE WS-TAX-RAW-3(WS-WORK-INDEX:10)
                   TO WS-TAX-CODE(WS-TAX-IDX)
               MOVE WS-TAX-RAW-3(WS-WORK-INDEX + 10:40)
                   TO WS-TAX-SPECIALTY(WS-TAX-IDX)
               MOVE WS-TAX-RAW-3(WS-WORK-INDEX + 50:2)
                   TO WS-TAX-CATEGORY(WS-TAX-IDX)
               MOVE WS-TAX-RAW-3(WS-WORK-INDEX + 52:1)
                   TO WS-TAX-PCP-ELIGIBLE(WS-TAX-IDX)
               ADD 53 TO WS-WORK-INDEX
           END-PERFORM

           MOVE 225 TO WS-TAXONOMY-COUNT
           SET WS-TAXONOMY-LOADED TO TRUE

           DISPLAY 'HCPRVMNT - TAXONOMY TABLE LOADED: '
                   WS-TAXONOMY-COUNT ' ENTRIES'.

      ****************************************************************
      * 1020-LOAD-STATE-LICENSE-TABLE
      * LOADS STATE MEDICAL LICENSING BOARD INFORMATION FROM THE
      * STATE_LICENSE_BOARDS DATABASE TABLE. INCLUDES INTERSTATE
      * MEDICAL LICENSURE COMPACT MEMBERSHIP, CME REQUIREMENTS,
      * AND RENEWAL CYCLE INFORMATION FOR ALL 50 STATES + DC + TERR.
      ****************************************************************
       1020-LOAD-STATE-LICENSE-TABLE.

           INITIALIZE WS-STATE-LICENSE-TABLE
           MOVE ZERO TO WS-STATE-COUNT

           EXEC SQL
               DECLARE STATE_CURSOR CURSOR FOR
               SELECT STATE_CODE,
                      BOARD_NAME,
                      COMPACT_MEMBER_IND,
                      RENEWAL_CYCLE_YEARS,
                      CME_HOURS_REQUIRED,
                      DEA_REQUIRED_IND,
                      CSR_REQUIRED_IND
               FROM STATE_LICENSE_BOARDS
               WHERE ACTIVE_IND = 'Y'
               ORDER BY STATE_CODE
           END-EXEC

           EXEC SQL
               OPEN STATE_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCPRVMNT - WARNING: CANNOT LOAD STATE TABLE'
               DISPLAY 'HCPRVMNT - SQLCODE=' SQLCODE
           ELSE
               PERFORM VARYING WS-ST-IDX FROM 1 BY 1
                   UNTIL WS-ST-IDX > 55
                   EXEC SQL
                       FETCH STATE_CURSOR
                       INTO :WS-ST-CODE(WS-ST-IDX),
                            :WS-ST-BOARD-NAME(WS-ST-IDX),
                            :WS-ST-COMPACT-MEMBER(WS-ST-IDX),
                            :WS-ST-RENEWAL-CYCLE(WS-ST-IDX),
                            :WS-ST-CME-REQUIRED(WS-ST-IDX),
                            :WS-ST-DEA-REQUIRED(WS-ST-IDX),
                            :WS-ST-CSR-REQUIRED(WS-ST-IDX)
                   END-EXEC

                   IF SQLCODE = 0
                       ADD 1 TO WS-STATE-COUNT
                   ELSE
                       IF SQLCODE = 100
                           SET WS-ST-IDX TO 56
                       ELSE
                           DISPLAY 'HCPRVMNT - STATE FETCH ERROR '
                                   'SQLCODE=' SQLCODE
                           SET WS-ST-IDX TO 56
                       END-IF
                   END-IF
               END-PERFORM

               EXEC SQL
                   CLOSE STATE_CURSOR
               END-EXEC
           END-IF

           DISPLAY 'HCPRVMNT - STATE LICENSE TABLE LOADED: '
                   WS-STATE-COUNT ' ENTRIES'.

      ****************************************************************
      * 1030-LOAD-TERM-REASON-TABLE
      * LOADS 35 TERMINATION REASON CODES FROM WORKING-STORAGE
      * DATA AREA. EACH CODE INDICATES VOLUNTARY/INVOLUNTARY,
      * MEMBER NOTIFICATION REQUIREMENTS, RECOUPMENT FLAGS, AND
      * REGULATORY REPORTING REQUIREMENTS.
      ****************************************************************
       1030-LOAD-TERM-REASON-TABLE.

           INITIALIZE WS-TERM-REASON-TABLE
           MOVE 1 TO WS-WORK-INDEX

           PERFORM VARYING WS-TERM-IDX FROM 1 BY 1
               UNTIL WS-TERM-IDX > 35
               MOVE WS-TERM-REASON-DATA(WS-WORK-INDEX:3)
                   TO WS-TERM-CODE(WS-TERM-IDX)
               MOVE WS-TERM-REASON-DATA(WS-WORK-INDEX + 3:50)
                   TO WS-TERM-DESC(WS-TERM-IDX)
               MOVE WS-TERM-REASON-DATA(WS-WORK-INDEX + 53:1)
                   TO WS-TERM-VOLUNTARY-IND(WS-TERM-IDX)
               MOVE WS-TERM-REASON-DATA(WS-WORK-INDEX + 54:1)
                   TO WS-TERM-NOTIFY-REQ(WS-TERM-IDX)
               MOVE WS-TERM-REASON-DATA(WS-WORK-INDEX + 55:1)
                   TO WS-TERM-RECOUP-REQ(WS-TERM-IDX)
               ADD 56 TO WS-WORK-INDEX
           END-PERFORM

           DISPLAY 'HCPRVMNT - TERMINATION REASON TABLE LOADED: '
                   '35 ENTRIES'.

      ****************************************************************
      * 1040-LOAD-CONTRACT-RATE-TABLE
      * LOADS ACTIVE CONTRACT RATE SCHEDULES FROM THE DATABASE.
      * EACH ENTRY CONTAINS FEE SCHEDULE REFERENCES, NETWORK
      * ASSIGNMENTS, BASE REIMBURSEMENT PERCENTAGES, ESCALATION
      * FACTORS, WITHHOLD PARAMETERS, QUALITY BONUSES, AND
      * VALUE-BASED CONTRACT TERMS INCLUDING SHARED SAVINGS AND
      * RISK CORRIDOR BOUNDARIES.
      ****************************************************************
       1040-LOAD-CONTRACT-RATE-TABLE.

           INITIALIZE WS-CONTRACT-RATE-TABLE

           EXEC SQL
               DECLARE RATE_CURSOR CURSOR FOR
               SELECT FEE_SCHEDULE_ID,
                      NETWORK_ID,
                      TIER_LEVEL,
                      EFFECTIVE_DATE,
                      TERM_DATE,
                      BASE_REIMBURSE_PCT,
                      ANNUAL_ESCALATION_PCT,
                      WITHHOLD_PCT,
                      QUALITY_BONUS_PCT,
                      VBC_TARGET_AMOUNT,
                      SHARED_SAVINGS_PCT,
                      RISK_CORRIDOR_UPPER,
                      RISK_CORRIDOR_LOWER
               FROM CONTRACT_RATE_SCHEDULES
               WHERE ACTIVE_IND = 'Y'
                 AND TERM_DATE >= CURRENT_DATE
               ORDER BY NETWORK_ID, TIER_LEVEL
           END-EXEC

           EXEC SQL
               OPEN RATE_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCPRVMNT - WARNING: CANNOT LOAD RATE TABLE'
               DISPLAY 'HCPRVMNT - SQLCODE=' SQLCODE
           ELSE
               PERFORM VARYING WS-RATE-IDX FROM 1 BY 1
                   UNTIL WS-RATE-IDX > 50
                   EXEC SQL
                       FETCH RATE_CURSOR
                       INTO :WS-RATE-FEE-SCHED-ID(WS-RATE-IDX),
                            :WS-RATE-NETWORK-ID(WS-RATE-IDX),
                            :WS-RATE-TIER(WS-RATE-IDX),
                            :WS-RATE-EFF-DATE(WS-RATE-IDX),
                            :WS-RATE-TERM-DATE(WS-RATE-IDX),
                            :WS-RATE-BASE-PCT(WS-RATE-IDX),
                            :WS-RATE-ESCALATION(WS-RATE-IDX),
                            :WS-RATE-WITHHOLD-PCT(WS-RATE-IDX),
                            :WS-RATE-QUALITY-BONUS(WS-RATE-IDX),
                            :WS-RATE-VBC-TARGET(WS-RATE-IDX),
                            :WS-RATE-SHARED-SAV(WS-RATE-IDX),
                            :WS-RATE-RISK-UPPER(WS-RATE-IDX),
                            :WS-RATE-RISK-LOWER(WS-RATE-IDX)
                   END-EXEC

                   IF SQLCODE NOT = 0
                       IF SQLCODE = 100
                           SET WS-RATE-IDX TO 51
                       ELSE
                           DISPLAY 'HCPRVMNT - RATE FETCH ERROR '
                                   'SQLCODE=' SQLCODE
                           SET WS-RATE-IDX TO 51
                       END-IF
                   END-IF
               END-PERFORM

               EXEC SQL
                   CLOSE RATE_CURSOR
               END-EXEC
           END-IF

           DISPLAY 'HCPRVMNT - CONTRACT RATE TABLE LOADED'.

      ****************************************************************
      * 1050-LOAD-NETWORK-TIER-TABLE
      * LOADS NETWORK TIER DEFINITIONS USED TO DETERMINE PROVIDER
      * TIER PLACEMENT BASED ON QUALITY, COST, SATISFACTION, AND
      * CLINICAL OUTCOMES COMPOSITE SCORES.
      ****************************************************************
       1050-LOAD-NETWORK-TIER-TABLE.

           INITIALIZE WS-NETWORK-TIER-TABLE

           EXEC SQL
               DECLARE TIER_CURSOR CURSOR FOR
               SELECT NETWORK_ID,
                      TIER_CODE,
                      TIER_NAME,
                      MIN_QUALITY_SCORE,
                      MIN_COST_SCORE,
                      MIN_SATISFACTION_SCORE,
                      MIN_OUTCOME_SCORE,
                      MIN_COMPOSITE_SCORE
               FROM NETWORK_TIER_DEFINITIONS
               WHERE ACTIVE_IND = 'Y'
               ORDER BY NETWORK_ID, TIER_CODE
           END-EXEC

           EXEC SQL
               OPEN TIER_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCPRVMNT - WARNING: CANNOT LOAD TIER TABLE'
           ELSE
               PERFORM VARYING WS-TIER-IDX FROM 1 BY 1
                   UNTIL WS-TIER-IDX > 20
                   EXEC SQL
                       FETCH TIER_CURSOR
                       INTO :WS-TIER-NETWORK-ID(WS-TIER-IDX),
                            :WS-TIER-CODE(WS-TIER-IDX),
                            :WS-TIER-NAME(WS-TIER-IDX),
                            :WS-TIER-QUAL-MIN(WS-TIER-IDX),
                            :WS-TIER-COST-MIN(WS-TIER-IDX),
                            :WS-TIER-SAT-MIN(WS-TIER-IDX),
                            :WS-TIER-OUTCOME-MIN(WS-TIER-IDX),
                            :WS-TIER-COMPOSITE-MIN(WS-TIER-IDX)
                   END-EXEC

                   IF SQLCODE NOT = 0
                       SET WS-TIER-IDX TO 21
                   END-IF
               END-PERFORM

               EXEC SQL
                   CLOSE TIER_CURSOR
               END-EXEC
           END-IF

           DISPLAY 'HCPRVMNT - NETWORK TIER TABLE LOADED'.

      ****************************************************************
      * 1500-PROCESS-TRANSACTIONS
      * READS EACH TRANSACTION RECORD, VALIDATES BASIC STRUCTURE,
      * AND ROUTES TO THE APPROPRIATE PROCESSING PARAGRAPH BASED
      * ON TRANSACTION TYPE. HANDLES HEADER/TRAILER RECORDS AND
      * MAINTAINS RUNNING COUNTS.
      ****************************************************************
       1500-PROCESS-TRANSACTIONS.

           IF PTR-IS-TRAILER
               DISPLAY 'HCPRVMNT - TRAILER RECORD ENCOUNTERED'
               SET WS-END-OF-FILE TO TRUE
           ELSE
               IF PTR-IS-DETAIL
                   SET WS-VALID-TRANS TO TRUE
                   PERFORM 2000-PROCESS-PROVIDER-TRANSACTION
                   ADD 1 TO WS-TRANS-PROCESSED-CTR
               ELSE
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'TR0001' TO ERR-CODE
                   MOVE 'RECORD-TYPE' TO ERR-FIELD-NAME
                   MOVE PTR-RECORD-TYPE TO ERR-FIELD-VALUE
                   MOVE 'INVALID RECORD TYPE - NOT HD/DT/TR'
                       TO ERR-MESSAGE
                   MOVE '1500-PROCESS-TRANSACTIONS'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF

      * READ NEXT TRANSACTION
               READ PROVIDER-TRANS-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-TRANS-READ-CTR
               END-READ
           END-IF.

      ****************************************************************
      * 2000-PROCESS-PROVIDER-TRANSACTION
      * MASTER ROUTING PARAGRAPH - EXAMINES THE TRANSACTION TYPE
      * CODE AND DISPATCHES TO THE APPROPRIATE PROCESSING PARAGRAPH.
      * VALIDATES THAT REQUIRED FIELDS ARE PRESENT FOR EACH
      * TRANSACTION TYPE BEFORE ROUTING.
      ****************************************************************
       2000-PROCESS-PROVIDER-TRANSACTION.

           EVALUATE TRUE
               WHEN PTR-NEW-ENROLLMENT
                   IF PTR-NPI = SPACES OR PTR-TAX-ID = SPACES
                       MOVE 'E' TO ERR-SEVERITY
                       MOVE 'TR0010' TO ERR-CODE
                       MOVE 'NPI/TAX-ID' TO ERR-FIELD-NAME
                       MOVE PTR-NPI TO ERR-FIELD-VALUE
                       MOVE 'ENROLLMENT REQUIRES NPI AND TAX-ID'
                           TO ERR-MESSAGE
                       MOVE '2000-PROCESS-PROVIDER-TRANSACTION'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                   ELSE
                       PERFORM 2100-ENROLL-NEW-PROVIDER
                   END-IF

               WHEN PTR-DEMOGRAPHIC-UPD
                   IF PTR-PROVIDER-ID = SPACES AND
                      PTR-NPI = SPACES
                       MOVE 'E' TO ERR-SEVERITY
                       MOVE 'TR0020' TO ERR-CODE
                       MOVE 'PROVIDER-ID/NPI' TO ERR-FIELD-NAME
                       MOVE SPACES TO ERR-FIELD-VALUE
                       MOVE 'DEMOG UPDATE REQUIRES PROVIDER-ID OR NPI'
                           TO ERR-MESSAGE
                       MOVE '2000-PROCESS-PROVIDER-TRANSACTION'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                   ELSE
                       PERFORM 2200-UPDATE-PROVIDER-DEMOGRAPHICS
                   END-IF

               WHEN PTR-TERMINATION
                   IF PTR-TERM-REASON-CODE = SPACES
                       MOVE 'E' TO ERR-SEVERITY
                       MOVE 'TR0030' TO ERR-CODE
                       MOVE 'TERM-REASON-CODE' TO ERR-FIELD-NAME
                       MOVE PTR-TERM-REASON-CODE TO ERR-FIELD-VALUE
                       MOVE 'TERMINATION REQUIRES REASON CODE'
                           TO ERR-MESSAGE
                       MOVE '2000-PROCESS-PROVIDER-TRANSACTION'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                   ELSE
                       PERFORM 2300-TERMINATE-PROVIDER
                   END-IF

               WHEN PTR-REACTIVATION
                   PERFORM 2400-PROVIDER-REACTIVATION

               WHEN PTR-CREDENTIAL-UPD
                   PERFORM 3000-CREDENTIALING-PROCESS

               WHEN PTR-NETWORK-CHANGE
                   PERFORM 4000-NETWORK-ASSIGNMENT

               WHEN PTR-PAYMENT-SETUP
                   PERFORM 5000-SETUP-PAYMENT-METHOD

               WHEN PTR-ADDRESS-CHANGE
                   PERFORM 2200-UPDATE-PROVIDER-DEMOGRAPHICS

               WHEN PTR-SPECIALTY-ADD
                   PERFORM 2200-UPDATE-PROVIDER-DEMOGRAPHICS

               WHEN PTR-CONTRACT-CHANGE
                   PERFORM 4400-CONTRACT-MANAGEMENT

               WHEN OTHER
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'TR0099' TO ERR-CODE
                   MOVE 'TRANS-TYPE' TO ERR-FIELD-NAME
                   MOVE PTR-TRANS-TYPE TO ERR-FIELD-VALUE
                   MOVE 'UNKNOWN TRANSACTION TYPE CODE'
                       TO ERR-MESSAGE
                   MOVE '2000-PROCESS-PROVIDER-TRANSACTION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
           END-EVALUATE.

      ****************************************************************
      * 2100-ENROLL-NEW-PROVIDER
      * PROCESSES A NEW PROVIDER ENROLLMENT. PERFORMS COMPREHENSIVE
      * VALIDATION: NPI LUHN CHECK DIGIT, NPPES CROSS-REFERENCE,
      * TAX ID VALIDATION, STATE LICENSE VERIFICATION, DEA NUMBER
      * VALIDATION, SPECIALTY TAXONOMY CODE VALIDATION, ENTITY TYPE
      * DETERMINATION. GEOCODES PRACTICE LOCATION VIA ZIP-TO-COUNTY
      * MAPPING. ASSIGNS INITIAL CREDENTIALING STATUS. ASSOCIATES
      * PROVIDER AGREEMENT AND CONTRACT. INSERTS INTO PROVIDER_MASTER
      * WITH FULL AUDIT TRAIL.
      ****************************************************************
       2100-ENROLL-NEW-PROVIDER.

      * FIRST CHECK IF PROVIDER ALREADY EXISTS BY NPI
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM PROVIDER_MASTER
               WHERE NPI = :PTR-NPI
                 AND STATUS NOT IN ('TM', 'IN')
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           ELSE
               IF HV-ROW-COUNT > 0
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0001' TO ERR-CODE
                   MOVE 'NPI' TO ERR-FIELD-NAME
                   MOVE PTR-NPI TO ERR-FIELD-VALUE
                   MOVE 'PROVIDER ALREADY EXISTS WITH THIS NPI'
                       TO ERR-MESSAGE
                   MOVE '2100-ENROLL-NEW-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2100-EXIT
               END-IF
           END-IF

      * VALIDATE NPI VIA LUHN CHECK DIGIT ALGORITHM
           MOVE PTR-NPI TO WS-NPI-WORK
           PERFORM 2110-VALIDATE-NPI

           IF WS-NPI-INVALID
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'EN0010' TO ERR-CODE
               MOVE 'NPI' TO ERR-FIELD-NAME
               MOVE PTR-NPI TO ERR-FIELD-VALUE
               MOVE 'NPI FAILS LUHN CHECK DIGIT VALIDATION'
                   TO ERR-MESSAGE
               MOVE '2100-ENROLL-NEW-PROVIDER'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 2100-EXIT
           END-IF

      * CROSS-REFERENCE NPI AGAINST NPPES REGISTRY
           PERFORM 2115-VERIFY-NPI-NPPES

           IF WS-NPI-NPPES-NOTFOUND
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'EN0011' TO ERR-CODE
               MOVE 'NPI' TO ERR-FIELD-NAME
               MOVE PTR-NPI TO ERR-FIELD-VALUE
               MOVE 'NPI NOT FOUND IN NPPES REGISTRY - MANUAL REVIEW'
                   TO ERR-MESSAGE
               MOVE '2100-ENROLL-NEW-PROVIDER'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * VALIDATE TAX ID (SSN OR EIN FORMAT)
           PERFORM 2120-VALIDATE-TAX-ID

      * VALIDATE STATE LICENSE
           IF PTR-LICENSE-NUMBER NOT = SPACES
               PERFORM 2125-VALIDATE-LICENSE
               IF WS-LICENSE-INVALID
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0020' TO ERR-CODE
                   MOVE 'LICENSE' TO ERR-FIELD-NAME
                   MOVE PTR-LICENSE-NUMBER TO ERR-FIELD-VALUE
                   MOVE 'STATE LICENSE VALIDATION FAILED'
                       TO ERR-MESSAGE
                   MOVE '2100-ENROLL-NEW-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2100-EXIT
               END-IF
           END-IF

      * VALIDATE DEA NUMBER IF PROVIDED
           IF PTR-DEA-NUMBER NOT = SPACES
               MOVE PTR-DEA-NUMBER TO WS-DEA-WORK
               PERFORM 2130-VALIDATE-DEA
               IF WS-DEA-INVALID
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0030' TO ERR-CODE
                   MOVE 'DEA-NUMBER' TO ERR-FIELD-NAME
                   MOVE PTR-DEA-NUMBER TO ERR-FIELD-VALUE
                   MOVE 'DEA NUMBER CHECK DIGIT VALIDATION FAILED'
                       TO ERR-MESSAGE
                   MOVE '2100-ENROLL-NEW-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2100-EXIT
               END-IF
           END-IF

      * VALIDATE TAXONOMY CODE AGAINST TABLE
           IF PTR-TAXONOMY-CODE NOT = SPACES
               PERFORM 2135-VALIDATE-TAXONOMY
               IF WS-TAXONOMY-INVALID
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0040' TO ERR-CODE
                   MOVE 'TAXONOMY-CODE' TO ERR-FIELD-NAME
                   MOVE PTR-TAXONOMY-CODE TO ERR-FIELD-VALUE
                   MOVE 'TAXONOMY CODE NOT FOUND IN VALID CODE TABLE'
                       TO ERR-MESSAGE
                   MOVE '2100-ENROLL-NEW-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2100-EXIT
               END-IF
           END-IF

      * DETERMINE ENTITY TYPE
           IF PTR-ENTITY-TYPE = '1'
               IF PTR-LAST-NAME = SPACES OR PTR-FIRST-NAME = SPACES
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0050' TO ERR-CODE
                   MOVE 'ENTITY-TYPE' TO ERR-FIELD-NAME
                   MOVE '1' TO ERR-FIELD-VALUE
                   MOVE 'TYPE 1 INDIVIDUAL REQUIRES LAST/FIRST NAME'
                       TO ERR-MESSAGE
                   MOVE '2100-ENROLL-NEW-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2100-EXIT
               END-IF
           ELSE
               IF PTR-ENTITY-TYPE = '2'
                   IF PTR-ORG-NAME = SPACES
                       MOVE 'E' TO ERR-SEVERITY
                       MOVE 'EN0051' TO ERR-CODE
                       MOVE 'ENTITY-TYPE' TO ERR-FIELD-NAME
                       MOVE '2' TO ERR-FIELD-VALUE
                       MOVE 'TYPE 2 ORGANIZATION REQUIRES ORG NAME'
                           TO ERR-MESSAGE
                       MOVE '2100-ENROLL-NEW-PROVIDER'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                       GO TO 2100-EXIT
                   END-IF
               ELSE
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0052' TO ERR-CODE
                   MOVE 'ENTITY-TYPE' TO ERR-FIELD-NAME
                   MOVE PTR-ENTITY-TYPE TO ERR-FIELD-VALUE
                   MOVE 'INVALID ENTITY TYPE - MUST BE 1 OR 2'
                       TO ERR-MESSAGE
                   MOVE '2100-ENROLL-NEW-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2100-EXIT
               END-IF
           END-IF

      * GEOCODE PRACTICE LOCATION VIA ZIP-TO-COUNTY MAPPING
           IF PTR-PRACTICE-ZIP NOT = SPACES
               MOVE PTR-PRACTICE-ZIP(1:5) TO WS-ZIP-INPUT
               PERFORM 2140-GEOCODE-ZIP-TO-COUNTY
           END-IF

      * DETERMINE IF THIS IS A PCP-ELIGIBLE SPECIALTY
           SET WS-NOT-PCP TO TRUE
           SET WS-NOT-PEDIATRIC TO TRUE
           PERFORM VARYING WS-TAX-IDX FROM 1 BY 1
               UNTIL WS-TAX-IDX > WS-TAXONOMY-COUNT
               IF WS-TAX-CODE(WS-TAX-IDX) = PTR-TAXONOMY-CODE
                   IF WS-TAX-PCP-ELIGIBLE(WS-TAX-IDX) = 'Y'
                       SET WS-IS-PCP TO TRUE
                   END-IF
                   IF WS-TAX-PEDIATRIC-IND(WS-TAX-IDX) = 'Y'
                       SET WS-IS-PEDIATRIC TO TRUE
                   END-IF
               END-IF
           END-PERFORM

      * GENERATE NEXT PROVIDER-ID SEQUENCE
           EXEC SQL
               SELECT NEXT_PROVIDER_SEQ
               INTO :HV-NEXT-PROVIDER-ID
               FROM SYSTEM_SEQUENCES
               WHERE SEQ_NAME = 'PROVIDER_ID'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-PROVIDER-SEQ-CTR
               STRING 'PRV' WS-PROVIDER-SEQ-CTR
                   DELIMITED BY SIZE INTO HV-NEXT-PROVIDER-ID
               END-STRING
           ELSE
               EXEC SQL
                   UPDATE SYSTEM_SEQUENCES
                   SET NEXT_PROVIDER_SEQ =
                       NEXT_PROVIDER_SEQ + 1,
                       LAST_UPDATE_DATE = CURRENT_TIMESTAMP
                   WHERE SEQ_NAME = 'PROVIDER_ID'
               END-EXEC
           END-IF

      * INSERT INTO PROVIDER_MASTER TABLE
           MOVE HV-NEXT-PROVIDER-ID TO HV-PROVIDER-ID
           MOVE PTR-NPI             TO HV-NPI
           MOVE PTR-ENTITY-TYPE     TO HV-ENTITY-TYPE
           MOVE PTR-TAX-ID          TO HV-TAX-ID
           MOVE PTR-LAST-NAME       TO HV-LAST-NAME
           MOVE PTR-FIRST-NAME      TO HV-FIRST-NAME
           MOVE PTR-MIDDLE-NAME     TO HV-MIDDLE-NAME
           MOVE PTR-ORG-NAME        TO HV-ORG-NAME
           MOVE PTR-DOB             TO HV-DOB
           MOVE PTR-GENDER          TO HV-GENDER
           MOVE 'AC'                TO HV-STATUS
           MOVE 'PD'                TO HV-CRED-STATUS
           MOVE PTR-NETWORK-ID      TO HV-NETWORK-ID
           MOVE PTR-TIER-LEVEL      TO HV-TIER-LEVEL
           MOVE PTR-SPECIALTY-CODE  TO HV-SPECIALTY-CODE
           MOVE PTR-SECONDARY-SPEC  TO HV-SECONDARY-SPEC
           MOVE PTR-TERTIARY-SPEC   TO HV-TERTIARY-SPEC
           MOVE PTR-TAXONOMY-CODE   TO HV-TAXONOMY-CODE
           MOVE PTR-LICENSE-STATE   TO HV-LICENSE-STATE
           MOVE PTR-LICENSE-NUMBER  TO HV-LICENSE-NUMBER
           MOVE PTR-LICENSE-EXP-DATE TO HV-LICENSE-EXP-DATE
           MOVE PTR-DEA-NUMBER      TO HV-DEA-NUMBER
           MOVE PTR-DEA-EXP-DATE    TO HV-DEA-EXP-DATE
           MOVE PTR-MEDICARE-ID     TO HV-MEDICARE-ID
           MOVE PTR-MEDICAID-ID     TO HV-MEDICAID-ID
           MOVE PTR-PRACTICE-ADDR-1 TO HV-PRACTICE-ADDR-1
           MOVE PTR-PRACTICE-ADDR-2 TO HV-PRACTICE-ADDR-2
           MOVE PTR-PRACTICE-CITY   TO HV-PRACTICE-CITY
           MOVE PTR-PRACTICE-STATE  TO HV-PRACTICE-STATE
           MOVE PTR-PRACTICE-ZIP    TO HV-PRACTICE-ZIP

           IF WS-ZIP-COUNTY-CODE NOT = SPACES
               MOVE WS-ZIP-COUNTY-CODE TO HV-PRACTICE-COUNTY
           ELSE
               MOVE PTR-PRACTICE-COUNTY TO HV-PRACTICE-COUNTY
           END-IF

           MOVE PTR-PRACTICE-PHONE  TO HV-PRACTICE-PHONE
           MOVE PTR-PRACTICE-FAX    TO HV-PRACTICE-FAX
           MOVE PTR-PRACTICE-EMAIL  TO HV-PRACTICE-EMAIL
           MOVE PTR-CONTRACT-ID     TO HV-CONTRACT-ID
           MOVE PTR-CONTRACT-EFF-DATE TO HV-CONTRACT-EFF-DATE
           MOVE PTR-CONTRACT-TERM-DATE TO HV-CONTRACT-TERM-DATE
           MOVE PTR-FEE-SCHED-ID    TO HV-FEE-SCHED-ID
           MOVE 'C'                 TO HV-PAY-METHOD
           MOVE 'N'                 TO HV-EFT-STATUS
           MOVE PTR-WITHHOLD-PCT    TO HV-WITHHOLD-PCT
           MOVE PTR-QUALITY-BONUS-FLAG TO HV-QUALITY-BONUS-FLAG
           MOVE PTR-VBC-FLAG        TO HV-VBC-FLAG
           MOVE PTR-SHARED-SAVINGS-PCT TO HV-SHARED-SAVINGS-PCT

           IF WS-IS-PCP
               MOVE ZERO             TO HV-PCP-PANEL-SIZE
               MOVE 2500             TO HV-PCP-PANEL-MAX
               MOVE 'Y'              TO HV-ACCEPTING-NEW
           ELSE
               MOVE ZERO             TO HV-PCP-PANEL-SIZE
               MOVE ZERO             TO HV-PCP-PANEL-MAX
               MOVE 'N'              TO HV-ACCEPTING-NEW
           END-IF

           MOVE WS-FORMATTED-DATE   TO HV-CRED-DATE
           MOVE SPACES               TO HV-RECRED-DUE-DATE
           MOVE ZERO                 TO HV-CRED-SCORE
           MOVE ZERO                 TO HV-QUALITY-SCORE
           MOVE WS-FORMATTED-DATE   TO HV-OIG-CHECK-DATE
           MOVE WS-FORMATTED-DATE   TO HV-SAM-CHECK-DATE
           MOVE PTR-CONTRACT-EFF-DATE TO HV-EFF-DATE
           MOVE SPACES               TO HV-TERM-DATE
           MOVE SPACES               TO HV-TERM-REASON
           MOVE WS-FORMATTED-DATE   TO HV-LAST-UPDATE-DATE
           MOVE PTR-USER-ID         TO HV-LAST-UPDATE-USER
           MOVE WS-PROGRAM-NAME     TO HV-LAST-UPDATE-PGM

           EXEC SQL
               INSERT INTO PROVIDER_MASTER
               (PROVIDER_ID, NPI, ENTITY_TYPE, TAX_ID,
                LAST_NAME, FIRST_NAME, MIDDLE_NAME, ORG_NAME,
                DOB, GENDER, STATUS, CRED_STATUS,
                NETWORK_ID, TIER_LEVEL,
                SPECIALTY_CODE, SECONDARY_SPEC, TERTIARY_SPEC,
                TAXONOMY_CODE, LICENSE_STATE, LICENSE_NUMBER,
                LICENSE_EXP_DATE, DEA_NUMBER, DEA_EXP_DATE,
                MEDICARE_ID, MEDICAID_ID,
                PRACTICE_ADDR_1, PRACTICE_ADDR_2,
                PRACTICE_CITY, PRACTICE_STATE, PRACTICE_ZIP,
                PRACTICE_COUNTY, PRACTICE_PHONE, PRACTICE_FAX,
                PRACTICE_EMAIL,
                CONTRACT_ID, CONTRACT_EFF_DATE, CONTRACT_TERM_DATE,
                FEE_SCHEDULE_ID, PAY_METHOD, EFT_STATUS,
                WITHHOLD_PCT, QUALITY_BONUS_FLAG,
                VBC_FLAG, SHARED_SAVINGS_PCT,
                PCP_PANEL_SIZE, PCP_PANEL_MAX, ACCEPTING_NEW,
                CRED_DATE, RECRED_DUE_DATE,
                CRED_SCORE, QUALITY_SCORE,
                OIG_CHECK_DATE, SAM_CHECK_DATE,
                EFF_DATE, TERM_DATE, TERM_REASON,
                LAST_UPDATE_DATE, LAST_UPDATE_USER,
                LAST_UPDATE_PGM)
               VALUES
               (:HV-PROVIDER-ID, :HV-NPI, :HV-ENTITY-TYPE,
                :HV-TAX-ID,
                :HV-LAST-NAME, :HV-FIRST-NAME, :HV-MIDDLE-NAME,
                :HV-ORG-NAME,
                :HV-DOB, :HV-GENDER, :HV-STATUS, :HV-CRED-STATUS,
                :HV-NETWORK-ID, :HV-TIER-LEVEL,
                :HV-SPECIALTY-CODE, :HV-SECONDARY-SPEC,
                :HV-TERTIARY-SPEC,
                :HV-TAXONOMY-CODE, :HV-LICENSE-STATE,
                :HV-LICENSE-NUMBER,
                :HV-LICENSE-EXP-DATE, :HV-DEA-NUMBER,
                :HV-DEA-EXP-DATE,
                :HV-MEDICARE-ID, :HV-MEDICAID-ID,
                :HV-PRACTICE-ADDR-1, :HV-PRACTICE-ADDR-2,
                :HV-PRACTICE-CITY, :HV-PRACTICE-STATE,
                :HV-PRACTICE-ZIP,
                :HV-PRACTICE-COUNTY, :HV-PRACTICE-PHONE,
                :HV-PRACTICE-FAX,
                :HV-PRACTICE-EMAIL,
                :HV-CONTRACT-ID, :HV-CONTRACT-EFF-DATE,
                :HV-CONTRACT-TERM-DATE,
                :HV-FEE-SCHED-ID, :HV-PAY-METHOD, :HV-EFT-STATUS,
                :HV-WITHHOLD-PCT, :HV-QUALITY-BONUS-FLAG,
                :HV-VBC-FLAG, :HV-SHARED-SAVINGS-PCT,
                :HV-PCP-PANEL-SIZE, :HV-PCP-PANEL-MAX,
                :HV-ACCEPTING-NEW,
                :HV-CRED-DATE, :HV-RECRED-DUE-DATE,
                :HV-CRED-SCORE, :HV-QUALITY-SCORE,
                :HV-OIG-CHECK-DATE, :HV-SAM-CHECK-DATE,
                :HV-EFF-DATE, :HV-TERM-DATE, :HV-TERM-REASON,
                :HV-LAST-UPDATE-DATE, :HV-LAST-UPDATE-USER,
                :HV-LAST-UPDATE-PGM)
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
               GO TO 2100-EXIT
           END-IF

      * WRITE TO PROVIDER MASTER OUTPUT FILE
           MOVE HV-PROVIDER-ID      TO PMR-PROVIDER-ID
           MOVE HV-NPI              TO PMR-NPI
           MOVE HV-ENTITY-TYPE      TO PMR-ENTITY-TYPE
           MOVE HV-TAX-ID           TO PMR-TAX-ID
           MOVE HV-LAST-NAME        TO PMR-LAST-NAME
           MOVE HV-FIRST-NAME       TO PMR-FIRST-NAME
           MOVE HV-MIDDLE-NAME      TO PMR-MIDDLE-NAME
           MOVE HV-ORG-NAME         TO PMR-ORG-NAME
           MOVE HV-STATUS           TO PMR-STATUS
           MOVE HV-CRED-STATUS      TO PMR-CRED-STATUS
           MOVE HV-NETWORK-ID       TO PMR-NETWORK-ID
           MOVE HV-TIER-LEVEL       TO PMR-TIER-LEVEL
           MOVE HV-SPECIALTY-CODE   TO PMR-SPECIALTY-CODE
           MOVE HV-TAXONOMY-CODE    TO PMR-TAXONOMY-CODE
           MOVE HV-LICENSE-STATE    TO PMR-LICENSE-STATE
           MOVE HV-LICENSE-NUMBER   TO PMR-LICENSE-NUMBER
           MOVE HV-LICENSE-EXP-DATE TO PMR-LICENSE-EXP-DATE
           MOVE HV-DEA-NUMBER       TO PMR-DEA-NUMBER
           MOVE HV-CONTRACT-ID      TO PMR-CONTRACT-ID
           MOVE HV-PRACTICE-ADDR-1  TO PMR-PRACTICE-ADDR-1
           MOVE HV-PRACTICE-CITY    TO PMR-PRACTICE-CITY
           MOVE HV-PRACTICE-STATE   TO PMR-PRACTICE-STATE
           MOVE HV-PRACTICE-ZIP     TO PMR-PRACTICE-ZIP
           MOVE HV-PRACTICE-COUNTY  TO PMR-PRACTICE-COUNTY
           MOVE HV-EFF-DATE         TO PMR-EFF-DATE
           MOVE SPACES              TO PMR-TERM-DATE
           MOVE HV-CRED-DATE        TO PMR-CRED-DATE
           MOVE HV-RECRED-DUE-DATE  TO PMR-RECRED-DUE-DATE
           MOVE HV-PAY-METHOD       TO PMR-PAY-METHOD
           MOVE HV-PCP-PANEL-SIZE   TO PMR-PCP-PANEL-SIZE
           MOVE HV-PCP-PANEL-MAX    TO PMR-PCP-PANEL-MAX
           MOVE HV-ACCEPTING-NEW    TO PMR-ACCEPTING-NEW
           MOVE HV-OIG-CHECK-DATE   TO PMR-OIG-CHECK-DATE
           MOVE HV-SAM-CHECK-DATE   TO PMR-SAM-CHECK-DATE
           MOVE HV-CRED-SCORE       TO PMR-CRED-SCORE
           MOVE HV-QUALITY-SCORE    TO PMR-QUALITY-SCORE
           MOVE HV-LAST-UPDATE-DATE TO PMR-LAST-UPDATE-DATE
           MOVE HV-LAST-UPDATE-USER TO PMR-LAST-UPDATE-USER
           MOVE 'A'                 TO PMR-ACTION-CODE

           WRITE PROVIDER-MASTER-REC
           ADD 1 TO WS-MASTER-WRITTEN-CTR

      * WRITE AUDIT TRAIL RECORD
           MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
           MOVE HV-PROVIDER-ID      TO AUD-PROVIDER-ID
           MOVE HV-NPI              TO AUD-NPI
           MOVE 'ENR'               TO AUD-ACTION-TYPE
           MOVE PTR-TRANS-SEQ-NO    TO AUD-TRANS-SEQ
           MOVE PTR-USER-ID         TO AUD-USER-ID
           MOVE PTR-SOURCE-SYSTEM   TO AUD-SOURCE-SYSTEM
           MOVE 'PROVIDER_MASTER'   TO AUD-TABLE-NAME
           MOVE 'ALL FIELDS'        TO AUD-FIELD-NAME
           MOVE SPACES              TO AUD-BEFORE-VALUE
           MOVE 'NEW ENROLLMENT'    TO AUD-AFTER-VALUE
           MOVE PTR-CONTRACT-EFF-DATE TO AUD-EFF-DATE
           MOVE '00'                TO AUD-RESULT-CODE
           MOVE 'PROVIDER ENROLLED SUCCESSFULLY' TO AUD-RESULT-MSG

           WRITE AUDIT-REC
           ADD 1 TO WS-AUDIT-CTR

      * TRIGGER INITIAL CREDENTIALING
           PERFORM 3000-CREDENTIALING-PROCESS

           ADD 1 TO WS-ENROLL-CTR.

       2100-EXIT.
           EXIT.

      ****************************************************************
      * 2110-VALIDATE-NPI
      * VALIDATES AN NPI NUMBER USING THE LUHN MOD-10 CHECK DIGIT
      * ALGORITHM. THE NPI IS A 10-DIGIT NUMBER WHERE THE CHECK
      * DIGIT IS THE LAST DIGIT. THE ALGORITHM PREPENDS THE
      * HEALTH INDUSTRY NUMBER (80840) TO CREATE A 15-DIGIT STRING,
      * THEN APPLIES STANDARD LUHN VALIDATION.
      ****************************************************************
       2110-VALIDATE-NPI.

           SET WS-NPI-INVALID TO TRUE

      * VERIFY NPI IS 10 DIGITS AND NUMERIC
           IF WS-NPI-WORK IS NOT NUMERIC
               GO TO 2110-EXIT
           END-IF

      * NPI MUST START WITH 1 OR 2
           IF WS-NPI-WORK(1:1) NOT = '1' AND
              WS-NPI-WORK(1:1) NOT = '2'
               GO TO 2110-EXIT
           END-IF

      * CONSTRUCT 15-DIGIT NUMBER: 80840 + NPI (FIRST 9) + CHECK
      * THE CHECK DIGIT IS NPI POSITION 10
           MOVE WS-NPI-WORK(10:1)  TO WS-NPI-CHECK-DIGIT

      * SET UP THE 15-DIGIT ARRAY FOR LUHN: PREFIX + 9 NPI DIGITS
           MOVE 8 TO WS-NPI-DIGIT(1)
           MOVE 0 TO WS-NPI-DIGIT(2)
           MOVE 8 TO WS-NPI-DIGIT(3)
           MOVE 4 TO WS-NPI-DIGIT(4)
           MOVE 0 TO WS-NPI-DIGIT(5)

           PERFORM VARYING WS-NPI-IDX FROM 1 BY 1
               UNTIL WS-NPI-IDX > 9
               MOVE WS-NPI-WORK(WS-NPI-IDX:1)
                   TO WS-NPI-DIGIT(WS-NPI-IDX + 5)
           END-PERFORM

      * APPLY LUHN ALGORITHM - STARTING FROM RIGHTMOST DIGIT
      * (POSITION 14, SINCE POSITION 15 IS THE CHECK DIGIT POSITION)
      * DOUBLE EVERY OTHER DIGIT STARTING FROM POSITION 14
           MOVE ZERO TO WS-NPI-SUM

           PERFORM VARYING WS-NPI-IDX FROM 14 BY -2
               UNTIL WS-NPI-IDX < 1
      * DOUBLE THIS DIGIT
               MULTIPLY WS-NPI-DIGIT(WS-NPI-IDX) BY 2
                   GIVING WS-NPI-DOUBLED
      * IF RESULT > 9, SUBTRACT 9 (EQUIVALENT TO ADDING DIGITS)
               IF WS-NPI-DOUBLED > 9
                   SUBTRACT 9 FROM WS-NPI-DOUBLED
               END-IF
               ADD WS-NPI-DOUBLED TO WS-NPI-SUM
           END-PERFORM

      * ADD THE NON-DOUBLED DIGITS (ODD POSITIONS FROM RIGHT)
           PERFORM VARYING WS-NPI-IDX FROM 13 BY -2
               UNTIL WS-NPI-IDX < 1
               ADD WS-NPI-DIGIT(WS-NPI-IDX) TO WS-NPI-SUM
           END-PERFORM

      * CALCULATE CHECK DIGIT: (10 - (SUM MOD 10)) MOD 10
           DIVIDE WS-NPI-SUM BY 10
               GIVING WS-WORK-QUOTIENT
               REMAINDER WS-NPI-MOD-RESULT
           END-DIVIDE

           IF WS-NPI-MOD-RESULT = 0
               MOVE 0 TO WS-NPI-CALC-CHECK
           ELSE
               SUBTRACT WS-NPI-MOD-RESULT FROM 10
                   GIVING WS-NPI-CALC-CHECK
           END-IF

      * COMPARE CALCULATED CHECK DIGIT WITH ACTUAL
           IF WS-NPI-CALC-CHECK = WS-NPI-CHECK-DIGIT
               SET WS-NPI-VALID TO TRUE
           END-IF.

       2110-EXIT.
           EXIT.

      ****************************************************************
      * 2115-VERIFY-NPI-NPPES
      * CROSS-REFERENCES THE NPI AGAINST THE NPPES (NATIONAL PLAN
      * AND PROVIDER ENUMERATION SYSTEM) DATABASE EXTRACT TABLE.
      * VERIFIES THAT THE NPI IS ACTIVELY ENUMERATED AND MATCHES
      * THE PROVIDER NAME AND ENTITY TYPE ON FILE.
      ****************************************************************
       2115-VERIFY-NPI-NPPES.

           SET WS-NPI-NPPES-NOTFOUND TO TRUE

           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM NPPES_NPI_REGISTRY
               WHERE NPI = :PTR-NPI
                 AND DEACTIVATION_DATE IS NULL
                 AND ENTITY_TYPE_CODE = :PTR-ENTITY-TYPE
           END-EXEC

           IF SQLCODE = 0
               IF HV-ROW-COUNT > 0
                   SET WS-NPI-NPPES-FOUND TO TRUE

      * ADDITIONALLY VERIFY NAME MATCHES FOR INDIVIDUALS
                   IF PTR-INDIVIDUAL
                       EXEC SQL
                           SELECT COUNT(*)
                           INTO :HV-ROW-COUNT
                           FROM NPPES_NPI_REGISTRY
                           WHERE NPI = :PTR-NPI
                             AND UPPER(PROVIDER_LAST_NAME) =
                                 UPPER(:PTR-LAST-NAME)
                             AND DEACTIVATION_DATE IS NULL
                       END-EXEC

                       IF SQLCODE = 0 AND HV-ROW-COUNT = 0
                           MOVE 'W' TO ERR-SEVERITY
                           MOVE 'EN0012' TO ERR-CODE
                           MOVE 'NPI-NAME' TO ERR-FIELD-NAME
                           MOVE PTR-LAST-NAME TO ERR-FIELD-VALUE
                           MOVE 'NPI FOUND BUT NAME MISMATCH '
                               'WITH NPPES - REVIEW REQUIRED'
                               TO ERR-MESSAGE
                           MOVE '2115-VERIFY-NPI-NPPES'
                               TO ERR-PARAGRAPH-NAME
                           PERFORM 8000-ERROR-HANDLER
                       END-IF
                   END-IF
               END-IF
           END-IF.

      ****************************************************************
      * 2120-VALIDATE-TAX-ID
      * VALIDATES THE TAX IDENTIFICATION NUMBER. FOR SSN (TYPE S):
      * VERIFIES 9-DIGIT FORMAT, NOT ALL ZEROS, NOT IN INVALID
      * RANGES (000, 666, 900-999 FOR FIRST 3 DIGITS). FOR EIN
      * (TYPE E): VERIFIES VALID PREFIX (01-06, 10-16, 20-27, ETC.).
      ****************************************************************
       2120-VALIDATE-TAX-ID.

           MOVE PTR-TAX-ID TO WS-TAX-ID-WORK

           IF WS-TAX-ID-WORK = SPACES OR WS-TAX-ID-WORK = ZEROS
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'EN0015' TO ERR-CODE
               MOVE 'TAX-ID' TO ERR-FIELD-NAME
               MOVE WS-TAX-ID-WORK TO ERR-FIELD-VALUE
               MOVE 'TAX ID IS BLANK OR ALL ZEROS'
                   TO ERR-MESSAGE
               MOVE '2120-VALIDATE-TAX-ID' TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 2120-EXIT
           END-IF

           IF WS-TAX-ID-WORK IS NOT NUMERIC
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'EN0016' TO ERR-CODE
               MOVE 'TAX-ID' TO ERR-FIELD-NAME
               MOVE WS-TAX-ID-WORK TO ERR-FIELD-VALUE
               MOVE 'TAX ID CONTAINS NON-NUMERIC CHARACTERS'
                   TO ERR-MESSAGE
               MOVE '2120-VALIDATE-TAX-ID' TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 2120-EXIT
           END-IF

           IF PTR-SSN
      * VALIDATE SSN FORMAT
      * AREA NUMBER (FIRST 3) CANNOT BE 000, 666, OR 900-999
               IF WS-TAX-ID-WORK(1:3) = '000' OR
                  WS-TAX-ID-WORK(1:3) = '666' OR
                  WS-TAX-ID-WORK(1:1) = '9'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0017' TO ERR-CODE
                   MOVE 'TAX-ID-SSN' TO ERR-FIELD-NAME
                   MOVE WS-TAX-ID-WORK(1:3) TO ERR-FIELD-VALUE
                   MOVE 'SSN AREA NUMBER INVALID (000/666/9XX)'
                       TO ERR-MESSAGE
                   MOVE '2120-VALIDATE-TAX-ID'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
      * GROUP NUMBER (4-5) CANNOT BE 00
               IF WS-TAX-ID-WORK(4:2) = '00'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0018' TO ERR-CODE
                   MOVE 'TAX-ID-SSN' TO ERR-FIELD-NAME
                   MOVE WS-TAX-ID-WORK(4:2) TO ERR-FIELD-VALUE
                   MOVE 'SSN GROUP NUMBER CANNOT BE 00'
                       TO ERR-MESSAGE
                   MOVE '2120-VALIDATE-TAX-ID'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
      * SERIAL NUMBER (6-9) CANNOT BE 0000
               IF WS-TAX-ID-WORK(6:4) = '0000'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0019' TO ERR-CODE
                   MOVE 'TAX-ID-SSN' TO ERR-FIELD-NAME
                   MOVE WS-TAX-ID-WORK(6:4) TO ERR-FIELD-VALUE
                   MOVE 'SSN SERIAL NUMBER CANNOT BE 0000'
                       TO ERR-MESSAGE
                   MOVE '2120-VALIDATE-TAX-ID'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           ELSE
               IF PTR-EIN
      * VALIDATE EIN FORMAT - FIRST 2 DIGITS ARE IRS CAMPUS CODE
                   EVALUATE WS-TAX-ID-WORK(1:2)
                       WHEN '01' THRU '06' CONTINUE
                       WHEN '10' THRU '16' CONTINUE
                       WHEN '20' THRU '27' CONTINUE
                       WHEN '30' THRU '39' CONTINUE
                       WHEN '40' THRU '48' CONTINUE
                       WHEN '50' THRU '59' CONTINUE
                       WHEN '60' THRU '68' CONTINUE
                       WHEN '70' THRU '79' CONTINUE
                       WHEN '80' THRU '88' CONTINUE
                       WHEN '90' THRU '92' CONTINUE
                       WHEN '93' THRU '99' CONTINUE
                       WHEN OTHER
                           MOVE 'E' TO ERR-SEVERITY
                           MOVE 'EN0016' TO ERR-CODE
                           MOVE 'TAX-ID-EIN' TO ERR-FIELD-NAME
                           MOVE WS-TAX-ID-WORK(1:2)
                               TO ERR-FIELD-VALUE
                           MOVE 'EIN PREFIX NOT A VALID IRS CAMPUS'
                               TO ERR-MESSAGE
                           MOVE '2120-VALIDATE-TAX-ID'
                               TO ERR-PARAGRAPH-NAME
                           PERFORM 8000-ERROR-HANDLER
                   END-EVALUATE
               END-IF
           END-IF.

       2120-EXIT.
           EXIT.

      ****************************************************************
      * 2125-VALIDATE-LICENSE
      * VERIFIES STATE MEDICAL LICENSE AGAINST THE STATE LICENSING
      * BOARD TABLE. CHECKS LICENSE NUMBER FORMAT BY STATE, VERIFIES
      * EXPIRATION DATE IS NOT PAST, CHECKS FOR LICENSE ACTIONS
      * (DISCIPLINE, PROBATION, SUSPENSION, REVOCATION) IN THE
      * LICENSE_ACTION_HISTORY TABLE.
      ****************************************************************
       2125-VALIDATE-LICENSE.

           SET WS-LICENSE-INVALID TO TRUE

      * VERIFY STATE IS IN OUR LICENSE TABLE
           SET WS-ST-IDX TO 1
           SEARCH WS-STATE-ENTRY
               AT END
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0021' TO ERR-CODE
                   MOVE 'LICENSE-STATE' TO ERR-FIELD-NAME
                   MOVE PTR-LICENSE-STATE TO ERR-FIELD-VALUE
                   MOVE 'LICENSE STATE NOT IN STATE BOARD TABLE'
                       TO ERR-MESSAGE
                   MOVE '2125-VALIDATE-LICENSE'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2125-EXIT
               WHEN WS-ST-CODE(WS-ST-IDX) = PTR-LICENSE-STATE
                   CONTINUE
           END-SEARCH

      * CHECK LICENSE EXPIRATION DATE
           IF PTR-LICENSE-EXP-DATE NOT = SPACES
               IF PTR-LICENSE-EXP-DATE < WS-FORMATTED-DATE
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'EN0022' TO ERR-CODE
                   MOVE 'LICENSE-EXP-DATE' TO ERR-FIELD-NAME
                   MOVE PTR-LICENSE-EXP-DATE TO ERR-FIELD-VALUE
                   MOVE 'STATE LICENSE HAS EXPIRED'
                       TO ERR-MESSAGE
                   MOVE '2125-VALIDATE-LICENSE'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2125-EXIT
               END-IF
           END-IF

      * CHECK FOR LICENSE ACTIONS IN DATABASE
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM LICENSE_ACTION_HISTORY
               WHERE LICENSE_NUMBER = :PTR-LICENSE-NUMBER
                 AND LICENSE_STATE = :PTR-LICENSE-STATE
                 AND ACTION_STATUS IN ('AC', 'PR', 'SU')
                 AND (RESOLUTION_DATE IS NULL
                      OR RESOLUTION_DATE >= CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               IF HV-ROW-COUNT > 0
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'EN0023' TO ERR-CODE
                   MOVE 'LICENSE-ACTION' TO ERR-FIELD-NAME
                   MOVE PTR-LICENSE-NUMBER TO ERR-FIELD-VALUE
                   MOVE 'LICENSE HAS ACTIVE ACTIONS - REVIEW REQUIRED'
                       TO ERR-MESSAGE
                   MOVE '2125-VALIDATE-LICENSE'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
      * LICENSE WITH ACTIONS MAY STILL BE VALID BUT FLAGGED
               END-IF
           END-IF

      * IF WE GET HERE WITHOUT EXITING, LICENSE IS VALID
           SET WS-LICENSE-VALID TO TRUE.

       2125-EXIT.
           EXIT.

      ****************************************************************
      * 2130-VALIDATE-DEA
      * VALIDATES A DEA (DRUG ENFORCEMENT ADMINISTRATION) REGISTRATION
      * NUMBER USING THE DEA CHECK DIGIT ALGORITHM. DEA FORMAT:
      * 2 LETTERS + 6 DIGITS + 1 CHECK DIGIT. FIRST LETTER INDICATES
      * REGISTRANT TYPE (A/B=PRACTITIONER, M=MID-LEVEL, P/R=DIST).
      * SECOND LETTER IS FIRST LETTER OF REGISTRANT LAST NAME.
      * CHECK DIGIT: ADD DIGITS 1,3,5 + 2*(DIGITS 2,4,6). LAST
      * DIGIT OF SUM MUST EQUAL THE CHECK DIGIT (DIGIT 7).
      ****************************************************************
       2130-VALIDATE-DEA.

           SET WS-DEA-INVALID TO TRUE

      * VERIFY FORMAT: 2 ALPHA + 7 NUMERIC
           MOVE WS-DEA-WORK(1:1) TO WS-DEA-LETTER-1
           MOVE WS-DEA-WORK(2:1) TO WS-DEA-LETTER-2
           MOVE WS-DEA-WORK(3:7) TO WS-DEA-DIGITS

      * VALIDATE FIRST LETTER - REGISTRANT TYPE
           IF NOT (WS-DEA-PRACTITIONER OR
                   WS-DEA-MID-LEVEL OR
                   WS-DEA-DISTRIBUTOR)
               GO TO 2130-EXIT
           END-IF

      * VALIDATE SECOND LETTER MATCHES LAST NAME INITIAL
           MOVE PTR-LAST-NAME(1:1) TO WS-DEA-LAST-NAME-INIT
           MOVE FUNCTION UPPER-CASE(WS-DEA-LAST-NAME-INIT)
               TO WS-DEA-LAST-NAME-INIT
           MOVE FUNCTION UPPER-CASE(WS-DEA-LETTER-2)
               TO WS-DEA-LETTER-2

           IF WS-DEA-LETTER-2 NOT = WS-DEA-LAST-NAME-INIT
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'EN0031' TO ERR-CODE
               MOVE 'DEA-LETTER-2' TO ERR-FIELD-NAME
               MOVE WS-DEA-LETTER-2 TO ERR-FIELD-VALUE
               MOVE 'DEA 2ND LETTER DOES NOT MATCH LAST NAME INIT'
                   TO ERR-MESSAGE
               MOVE '2130-VALIDATE-DEA' TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * VALIDATE DIGITS ARE NUMERIC
           IF WS-DEA-DIGITS IS NOT NUMERIC
               GO TO 2130-EXIT
           END-IF

      * EXTRACT INDIVIDUAL DIGITS
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 7
               MOVE WS-DEA-DIGITS(WS-WORK-INDEX:1)
                   TO WS-DEA-DIG(WS-WORK-INDEX)
           END-PERFORM

      * DEA CHECK DIGIT ALGORITHM
      * SUM OF DIGITS IN POSITIONS 1, 3, 5
           MOVE ZERO TO WS-DEA-SUM-ODD
           ADD WS-DEA-DIG(1) TO WS-DEA-SUM-ODD
           ADD WS-DEA-DIG(3) TO WS-DEA-SUM-ODD
           ADD WS-DEA-DIG(5) TO WS-DEA-SUM-ODD

      * SUM OF DIGITS IN POSITIONS 2, 4, 6 THEN MULTIPLY BY 2
           MOVE ZERO TO WS-DEA-SUM-EVEN
           ADD WS-DEA-DIG(2) TO WS-DEA-SUM-EVEN
           ADD WS-DEA-DIG(4) TO WS-DEA-SUM-EVEN
           ADD WS-DEA-DIG(6) TO WS-DEA-SUM-EVEN

           MULTIPLY WS-DEA-SUM-EVEN BY 2
               GIVING WS-DEA-SUM-EVEN-X2

      * TOTAL = ODD-SUM + (2 * EVEN-SUM)
           ADD WS-DEA-SUM-ODD TO WS-DEA-SUM-EVEN-X2
               GIVING WS-DEA-TOTAL

      * CHECK DIGIT IS LAST DIGIT OF TOTAL
           DIVIDE WS-DEA-TOTAL BY 10
               GIVING WS-WORK-QUOTIENT
               REMAINDER WS-DEA-CALC-CHECK
           END-DIVIDE

      * COMPARE WITH ACTUAL CHECK DIGIT (POSITION 7)
           IF WS-DEA-CALC-CHECK = WS-DEA-DIG(7)
               SET WS-DEA-VALID TO TRUE
           END-IF.

       2130-EXIT.
           EXIT.

      ****************************************************************
      * 2135-VALIDATE-TAXONOMY
      * VALIDATES THE PROVIDER TAXONOMY CODE AGAINST THE LOADED
      * TAXONOMY TABLE OF 225 VALID NUCC CODES. SETS PCP AND
      * PEDIATRIC FLAGS BASED ON THE MATCHING ENTRY.
      ****************************************************************
       2135-VALIDATE-TAXONOMY.

           SET WS-TAXONOMY-INVALID TO TRUE

           IF NOT WS-TAXONOMY-LOADED
               GO TO 2135-EXIT
           END-IF

           PERFORM VARYING WS-TAX-IDX FROM 1 BY 1
               UNTIL WS-TAX-IDX > WS-TAXONOMY-COUNT
               IF WS-TAX-CODE(WS-TAX-IDX) = PTR-TAXONOMY-CODE
                   SET WS-TAXONOMY-VALID TO TRUE
                   IF WS-TAX-PCP-ELIGIBLE(WS-TAX-IDX) = 'Y'
                       SET WS-IS-PCP TO TRUE
                   END-IF
                   IF WS-TAX-PEDIATRIC-IND(WS-TAX-IDX) = 'Y'
                       SET WS-IS-PEDIATRIC TO TRUE
                   END-IF
                   SET WS-TAX-IDX TO WS-TAXONOMY-COUNT
               END-IF
           END-PERFORM.

       2135-EXIT.
           EXIT.

      ****************************************************************
      * 2140-GEOCODE-ZIP-TO-COUNTY
      * PERFORMS ZIP-TO-COUNTY MAPPING BY LOOKING UP THE 5-DIGIT
      * ZIP CODE IN THE ZIP_COUNTY_MAPPING TABLE. RETURNS THE FIPS
      * COUNTY CODE, COUNTY NAME, STATE, URBAN/RURAL INDICATOR,
      * AND MSA CODE FOR NETWORK ADEQUACY CALCULATIONS.
      ****************************************************************
       2140-GEOCODE-ZIP-TO-COUNTY.

           INITIALIZE WS-ZIP-COUNTY-WORK
           MOVE PTR-PRACTICE-ZIP(1:5) TO WS-ZIP-INPUT

           EXEC SQL
               SELECT COUNTY_FIPS_CODE,
                      COUNTY_NAME,
                      STATE_CODE,
                      URBAN_RURAL_IND,
                      LATITUDE,
                      LONGITUDE,
                      MSA_CODE
               INTO :WS-ZIP-COUNTY-CODE,
                    :WS-ZIP-COUNTY-NAME,
                    :WS-ZIP-STATE,
                    :WS-ZIP-URBAN-RURAL,
                    :WS-ZIP-LATITUDE,
                    :WS-ZIP-LONGITUDE,
                    :WS-ZIP-MSA-CODE
               FROM ZIP_COUNTY_MAPPING
               WHERE ZIP_CODE = :WS-ZIP-INPUT
           END-EXEC

           IF SQLCODE NOT = 0
               IF SQLCODE = 100
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'GC0001' TO ERR-CODE
                   MOVE 'PRACTICE-ZIP' TO ERR-FIELD-NAME
                   MOVE WS-ZIP-INPUT TO ERR-FIELD-VALUE
                   MOVE 'ZIP CODE NOT FOUND IN COUNTY MAPPING TABLE'
                       TO ERR-MESSAGE
                   MOVE '2140-GEOCODE-ZIP-TO-COUNTY'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               ELSE
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF.

      ****************************************************************
      * 2200-UPDATE-PROVIDER-DEMOGRAPHICS
      * PROCESSES DEMOGRAPHIC CHANGES FOR AN EXISTING PROVIDER.
      * CAPTURES BEFORE-IMAGE, APPLIES CHANGES, PERFORMS NETWORK
      * IMPACT ANALYSIS FOR ADDRESS CHANGES, HANDLES RETROACTIVE
      * CHANGES, AND WRITES FULL BEFORE/AFTER AUDIT TRAIL.
      ****************************************************************
       2200-UPDATE-PROVIDER-DEMOGRAPHICS.

      * LOOK UP EXISTING PROVIDER RECORD
           IF PTR-PROVIDER-ID NOT = SPACES
               MOVE PTR-PROVIDER-ID TO HV-PROVIDER-ID
           ELSE
               EXEC SQL
                   SELECT PROVIDER_ID
                   INTO :HV-PROVIDER-ID
                   FROM PROVIDER_MASTER
                   WHERE NPI = :PTR-NPI
                     AND STATUS IN ('AC', 'SU')
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'DM0001' TO ERR-CODE
                   MOVE 'NPI' TO ERR-FIELD-NAME
                   MOVE PTR-NPI TO ERR-FIELD-VALUE
                   MOVE 'PROVIDER NOT FOUND FOR NPI' TO ERR-MESSAGE
                   MOVE '2200-UPDATE-PROVIDER-DEMOGRAPHICS'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2200-EXIT
               END-IF
           END-IF

      * FETCH CURRENT RECORD FOR BEFORE-IMAGE
           EXEC SQL
               SELECT PROVIDER_ID, LAST_NAME, FIRST_NAME,
                      ORG_NAME, PRACTICE_ADDR_1,
                      PRACTICE_CITY, PRACTICE_STATE, PRACTICE_ZIP,
                      PRACTICE_COUNTY, PRACTICE_PHONE, PRACTICE_FAX,
                      PRACTICE_EMAIL, SPECIALTY_CODE, TAXONOMY_CODE,
                      LICENSE_STATE, LICENSE_NUMBER,
                      NETWORK_ID, TIER_LEVEL, STATUS, CONTRACT_ID,
                      PAY_METHOD
               INTO :WS-BEF-PROVIDER-ID, :WS-BEF-LAST-NAME,
                    :WS-BEF-FIRST-NAME,
                    :WS-BEF-ORG-NAME, :WS-BEF-ADDR-1,
                    :WS-BEF-CITY, :WS-BEF-STATE, :WS-BEF-ZIP,
                    :WS-BEF-COUNTY, :WS-BEF-PHONE, :WS-BEF-FAX,
                    :WS-BEF-EMAIL, :WS-BEF-SPECIALTY,
                    :WS-BEF-TAXONOMY,
                    :WS-BEF-LICENSE-ST, :WS-BEF-LICENSE-NO,
                    :WS-BEF-NETWORK-ID, :WS-BEF-TIER,
                    :WS-BEF-STATUS, :WS-BEF-CONTRACT-ID,
                    :WS-BEF-PAY-METHOD
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
               GO TO 2200-EXIT
           END-IF

      * CHECK FOR RETROACTIVE EFFECTIVE DATE
           SET WS-NOT-RETROACTIVE TO TRUE
           IF PTR-TRANS-DATE NOT = SPACES
               IF PTR-TRANS-DATE < WS-FORMATTED-DATE
                   SET WS-IS-RETROACTIVE TO TRUE
                   ADD 1 TO WS-RETRO-CHANGE-CTR
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'DM0002' TO ERR-CODE
                   MOVE 'TRANS-DATE' TO ERR-FIELD-NAME
                   MOVE PTR-TRANS-DATE TO ERR-FIELD-VALUE
                   MOVE 'RETROACTIVE DEMOGRAPHIC CHANGE DETECTED'
                       TO ERR-MESSAGE
                   MOVE '2200-UPDATE-PROVIDER-DEMOGRAPHICS'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           END-IF

      * PROCESS NAME CHANGES
           IF PTR-LAST-NAME NOT = SPACES AND
              PTR-LAST-NAME NOT = WS-BEF-LAST-NAME
               MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
               MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
               MOVE PTR-NPI TO AUD-NPI
               MOVE 'DEM' TO AUD-ACTION-TYPE
               MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
               MOVE PTR-USER-ID TO AUD-USER-ID
               MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
               MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
               MOVE 'LAST_NAME' TO AUD-FIELD-NAME
               MOVE WS-BEF-LAST-NAME TO AUD-BEFORE-VALUE
               MOVE PTR-LAST-NAME TO AUD-AFTER-VALUE
               MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
               MOVE '00' TO AUD-RESULT-CODE
               MOVE 'NAME CHANGE PROCESSED' TO AUD-RESULT-MSG
               WRITE AUDIT-REC
               ADD 1 TO WS-AUDIT-CTR

               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET LAST_NAME = :PTR-LAST-NAME,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID,
                       LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

           IF PTR-FIRST-NAME NOT = SPACES AND
              PTR-FIRST-NAME NOT = WS-BEF-FIRST-NAME
               MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
               MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
               MOVE PTR-NPI TO AUD-NPI
               MOVE 'DEM' TO AUD-ACTION-TYPE
               MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
               MOVE PTR-USER-ID TO AUD-USER-ID
               MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
               MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
               MOVE 'FIRST_NAME' TO AUD-FIELD-NAME
               MOVE WS-BEF-FIRST-NAME TO AUD-BEFORE-VALUE
               MOVE PTR-FIRST-NAME TO AUD-AFTER-VALUE
               MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
               MOVE '00' TO AUD-RESULT-CODE
               MOVE 'FIRST NAME CHANGE PROCESSED' TO AUD-RESULT-MSG
               WRITE AUDIT-REC
               ADD 1 TO WS-AUDIT-CTR

               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET FIRST_NAME = :PTR-FIRST-NAME,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID,
                       LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

      * PROCESS ADDRESS CHANGE WITH NETWORK IMPACT ANALYSIS
           SET WS-NETWORK-UNCHANGED TO TRUE

           IF PTR-PRACTICE-ADDR-1 NOT = SPACES AND
              PTR-PRACTICE-ADDR-1 NOT = WS-BEF-ADDR-1
               ADD 1 TO WS-ADDR-CHANGE-CTR

      * GEOCODE NEW ADDRESS
               IF PTR-PRACTICE-ZIP NOT = SPACES
                   MOVE PTR-PRACTICE-ZIP(1:5) TO WS-ZIP-INPUT
                   PERFORM 2140-GEOCODE-ZIP-TO-COUNTY
               END-IF

      * CHECK IF COUNTY CHANGED - IMPACTS NETWORK ASSIGNMENT
               IF WS-ZIP-COUNTY-CODE NOT = SPACES AND
                  WS-ZIP-COUNTY-CODE NOT = WS-BEF-COUNTY
                   SET WS-NETWORK-CHANGED TO TRUE
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'DM0010' TO ERR-CODE
                   MOVE 'COUNTY-CHANGE' TO ERR-FIELD-NAME
                   STRING WS-BEF-COUNTY ' -> '
                          WS-ZIP-COUNTY-CODE
                       DELIMITED BY SIZE INTO ERR-FIELD-VALUE
                   END-STRING
                   MOVE 'ADDRESS CHANGE CROSSES COUNTY BOUNDARY '
                       '- NETWORK REASSIGNMENT MAY BE REQUIRED'
                       TO ERR-MESSAGE
                   MOVE '2200-UPDATE-PROVIDER-DEMOGRAPHICS'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF

      * AUDIT TRAIL FOR ADDRESS CHANGE
               MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
               MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
               MOVE PTR-NPI TO AUD-NPI
               MOVE 'DEM' TO AUD-ACTION-TYPE
               MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
               MOVE PTR-USER-ID TO AUD-USER-ID
               MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
               MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
               MOVE 'PRACTICE_ADDRESS' TO AUD-FIELD-NAME
               MOVE WS-BEF-ADDR-1 TO AUD-BEFORE-VALUE
               MOVE PTR-PRACTICE-ADDR-1 TO AUD-AFTER-VALUE
               MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
               MOVE '00' TO AUD-RESULT-CODE
               MOVE 'ADDRESS CHANGE PROCESSED' TO AUD-RESULT-MSG
               WRITE AUDIT-REC
               ADD 1 TO WS-AUDIT-CTR

               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET PRACTICE_ADDR_1 = :PTR-PRACTICE-ADDR-1,
                       PRACTICE_ADDR_2 = :PTR-PRACTICE-ADDR-2,
                       PRACTICE_CITY = :PTR-PRACTICE-CITY,
                       PRACTICE_STATE = :PTR-PRACTICE-STATE,
                       PRACTICE_ZIP = :PTR-PRACTICE-ZIP,
                       PRACTICE_COUNTY = :WS-ZIP-COUNTY-CODE,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID,
                       LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF

      * TRIGGER NETWORK REASSIGNMENT IF COUNTY CHANGED
               IF WS-NETWORK-CHANGED
                   PERFORM 4000-NETWORK-ASSIGNMENT
               END-IF
           END-IF

      * PROCESS PHONE/FAX/EMAIL UPDATES
           IF PTR-PRACTICE-PHONE NOT = SPACES AND
              PTR-PRACTICE-PHONE NOT = WS-BEF-PHONE
               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET PRACTICE_PHONE = :PTR-PRACTICE-PHONE,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID,
                       LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

           IF PTR-PRACTICE-FAX NOT = SPACES AND
              PTR-PRACTICE-FAX NOT = WS-BEF-FAX
               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET PRACTICE_FAX = :PTR-PRACTICE-FAX,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

           IF PTR-PRACTICE-EMAIL NOT = SPACES AND
              PTR-PRACTICE-EMAIL NOT = WS-BEF-EMAIL
               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET PRACTICE_EMAIL = :PTR-PRACTICE-EMAIL,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

      * PROCESS SPECIALTY CHANGE/ADDITION
           IF PTR-SPECIALTY-CODE NOT = SPACES AND
              PTR-SPECIALTY-CODE NOT = WS-BEF-SPECIALTY
               ADD 1 TO WS-SPEC-CHANGE-CTR

      * VALIDATE NEW TAXONOMY CODE
               IF PTR-TAXONOMY-CODE NOT = SPACES
                   PERFORM 2135-VALIDATE-TAXONOMY
                   IF WS-TAXONOMY-INVALID
                       MOVE 'E' TO ERR-SEVERITY
                       MOVE 'DM0020' TO ERR-CODE
                       MOVE 'TAXONOMY-CODE' TO ERR-FIELD-NAME
                       MOVE PTR-TAXONOMY-CODE TO ERR-FIELD-VALUE
                       MOVE 'NEW TAXONOMY CODE INVALID'
                           TO ERR-MESSAGE
                       MOVE '2200-UPDATE-PROVIDER-DEMOGRAPHICS'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                       GO TO 2200-EXIT
                   END-IF
               END-IF

               MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
               MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
               MOVE PTR-NPI TO AUD-NPI
               MOVE 'DEM' TO AUD-ACTION-TYPE
               MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
               MOVE PTR-USER-ID TO AUD-USER-ID
               MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
               MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
               MOVE 'SPECIALTY_CODE' TO AUD-FIELD-NAME
               MOVE WS-BEF-SPECIALTY TO AUD-BEFORE-VALUE
               MOVE PTR-SPECIALTY-CODE TO AUD-AFTER-VALUE
               MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
               MOVE '00' TO AUD-RESULT-CODE
               MOVE 'SPECIALTY CHANGE PROCESSED' TO AUD-RESULT-MSG
               WRITE AUDIT-REC
               ADD 1 TO WS-AUDIT-CTR

               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET SPECIALTY_CODE = :PTR-SPECIALTY-CODE,
                       TAXONOMY_CODE = :PTR-TAXONOMY-CODE,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID,
                       LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

           ADD 1 TO WS-DEMOG-UPD-CTR.

       2200-EXIT.
           EXIT.

      ****************************************************************
      * 2300-TERMINATE-PROVIDER
      * PROCESSES PROVIDER TERMINATION. DETERMINES VOLUNTARY VS
      * INVOLUNTARY BASED ON REASON CODE. HANDLES 30+ TERMINATION
      * REASONS. PROCESSES MEMBER NOTIFICATION REQUIREMENTS FOR PCP
      * TERMINATIONS, HANDLES CLAIMS IN-PROCESS, TRANSFERS OPEN
      * AUTHORIZATIONS, TRIGGERS FINAL PAYMENT PROCESSING AND 1099
      * GENERATION, SETS CONTRACT END DATE.
      ****************************************************************
       2300-TERMINATE-PROVIDER.

      * LOOK UP PROVIDER
           IF PTR-PROVIDER-ID NOT = SPACES
               MOVE PTR-PROVIDER-ID TO HV-PROVIDER-ID
           ELSE
               EXEC SQL
                   SELECT PROVIDER_ID
                   INTO :HV-PROVIDER-ID
                   FROM PROVIDER_MASTER
                   WHERE NPI = :PTR-NPI
                     AND STATUS = 'AC'
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'TM0001' TO ERR-CODE
                   MOVE 'NPI' TO ERR-FIELD-NAME
                   MOVE PTR-NPI TO ERR-FIELD-VALUE
                   MOVE 'ACTIVE PROVIDER NOT FOUND FOR TERMINATION'
                       TO ERR-MESSAGE
                   MOVE '2300-TERMINATE-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2300-EXIT
               END-IF
           END-IF

      * VALIDATE TERMINATION REASON CODE AGAINST TABLE
           SET WS-TERM-IDX TO 1
           SEARCH WS-TERM-ENTRY
               AT END
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'TM0002' TO ERR-CODE
                   MOVE 'TERM-REASON' TO ERR-FIELD-NAME
                   MOVE PTR-TERM-REASON-CODE TO ERR-FIELD-VALUE
                   MOVE 'INVALID TERMINATION REASON CODE'
                       TO ERR-MESSAGE
                   MOVE '2300-TERMINATE-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2300-EXIT
               WHEN WS-TERM-CODE(WS-TERM-IDX) =
                   PTR-TERM-REASON-CODE
                   CONTINUE
           END-SEARCH

      * SET EFFECTIVE DATE - USE PROVIDED DATE OR CURRENT
           IF PTR-TERM-EFF-DATE NOT = SPACES
               MOVE PTR-TERM-EFF-DATE TO HV-TERM-DATE
           ELSE
               MOVE WS-FORMATTED-DATE TO HV-TERM-DATE
           END-IF

      * CHECK FOR MEMBERS ASSIGNED TO THIS PROVIDER (PCP)
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-MEMBER-COUNT
               FROM MEMBER_PCP_ASSIGNMENT
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND STATUS = 'AC'
                 AND (TERM_DATE IS NULL
                      OR TERM_DATE >= CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0 AND HV-MEMBER-COUNT > 0
      * MEMBERS NEED TO BE REASSIGNED
               IF WS-TERM-NOTIFY-REQ(WS-TERM-IDX) = 'Y'
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'TM0010' TO ERR-CODE
                   MOVE 'MEMBER-COUNT' TO ERR-FIELD-NAME
                   MOVE HV-MEMBER-COUNT TO WS-SQLCODE-DISPLAY
                   MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
                   MOVE 'MEMBERS ASSIGNED TO TERMINATING PCP '
                       '- REASSIGNMENT AND NOTIFICATION REQUIRED'
                       TO ERR-MESSAGE
                   MOVE '2300-TERMINATE-PROVIDER'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF

      * INSERT MEMBER REASSIGNMENT QUEUE ENTRIES
               EXEC SQL
                   INSERT INTO MEMBER_REASSIGN_QUEUE
                   (PROVIDER_ID, MEMBER_COUNT,
                    REASON_CODE, EFFECTIVE_DATE,
                    NOTIFY_REQUIRED, QUEUE_DATE,
                    STATUS, CREATED_BY)
                   VALUES
                   (:HV-PROVIDER-ID, :HV-MEMBER-COUNT,
                    :PTR-TERM-REASON-CODE, :HV-TERM-DATE,
                    :PTR-TERM-NOTIFY-MEMBERS, CURRENT_TIMESTAMP,
                    'PD', :PTR-USER-ID)
               END-EXEC

               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF

               ADD HV-MEMBER-COUNT TO WS-PCP-REASSIGN-CTR
           END-IF

      * CHECK FOR CLAIMS IN PROCESS
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-CLAIMS-COUNT
               FROM CLAIMS_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND CLAIM_STATUS IN ('PD', 'SU', 'IP')
           END-EXEC

           IF SQLCODE = 0 AND HV-CLAIMS-COUNT > 0
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'TM0020' TO ERR-CODE
               MOVE 'CLAIMS-IN-PROCESS' TO ERR-FIELD-NAME
               MOVE HV-CLAIMS-COUNT TO WS-SQLCODE-DISPLAY
               MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
               MOVE 'CLAIMS IN PROCESS FOR TERMINATING PROVIDER'
                   TO ERR-MESSAGE
               MOVE '2300-TERMINATE-PROVIDER'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * CHECK FOR OPEN AUTHORIZATIONS AND TRANSFER
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-AUTH-COUNT
               FROM AUTHORIZATION_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND AUTH_STATUS = 'AP'
                 AND AUTH_EXP_DATE >= CURRENT_DATE
           END-EXEC

           IF SQLCODE = 0 AND HV-AUTH-COUNT > 0
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'TM0030' TO ERR-CODE
               MOVE 'OPEN-AUTHS' TO ERR-FIELD-NAME
               MOVE HV-AUTH-COUNT TO WS-SQLCODE-DISPLAY
               MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
               MOVE 'OPEN AUTHORIZATIONS REQUIRE TRANSFER'
                   TO ERR-MESSAGE
               MOVE '2300-TERMINATE-PROVIDER'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER

      * MARK AUTHS FOR PROVIDER TRANSFER REVIEW
               EXEC SQL
                   UPDATE AUTHORIZATION_MASTER
                   SET TRANSFER_REQUIRED = 'Y',
                       TRANSFER_REASON = 'PROVIDER TERMINATION',
                       LAST_UPDATE_DATE = CURRENT_TIMESTAMP,
                       LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
                     AND AUTH_STATUS = 'AP'
                     AND AUTH_EXP_DATE >= CURRENT_DATE
               END-EXEC

               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

      * TRIGGER FINAL PAYMENT PROCESSING
           EXEC SQL
               SELECT COALESCE(SUM(PAID_AMOUNT), 0)
               INTO :HV-YTD-TOTAL-PAID
               FROM CLAIMS_PAYMENT
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND PAYMENT_YEAR = YEAR(CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               MOVE HV-YTD-TOTAL-PAID TO WS-1099-YTD-PAYMENTS
               MOVE HV-PROVIDER-ID TO WS-1099-PROVIDER-ID

      * CHECK 1099 THRESHOLD
               IF WS-1099-YTD-PAYMENTS >= WS-1099-THRESHOLD
                   SET WS-1099-EXCEEDS-THRESH TO TRUE
                   ADD 1 TO WS-1099-TRIGGER-CTR

      * INSERT 1099 GENERATION TRIGGER
                   EXEC SQL
                       INSERT INTO TAX_1099_QUEUE
                       (PROVIDER_ID, TAX_ID, TAX_YEAR,
                        YTD_PAYMENTS, FORM_TYPE,
                        TRIGGER_REASON, TRIGGER_DATE,
                        STATUS)
                       VALUES
                       (:HV-PROVIDER-ID,
                        :PTR-TAX-ID,
                        YEAR(CURRENT_DATE),
                        :HV-YTD-TOTAL-PAID,
                        'NEC',
                        'TERMINATION',
                        CURRENT_TIMESTAMP,
                        'PD')
                   END-EXEC

                   IF SQLCODE NOT = 0
                       PERFORM 8100-DATABASE-ERROR
                   END-IF
               END-IF
           END-IF

      * DETERMINE RECOUPMENT REQUIREMENTS
           IF WS-TERM-RECOUP-REQ(WS-TERM-IDX) = 'Y'
               EXEC SQL
                   INSERT INTO RECOUPMENT_QUEUE
                   (PROVIDER_ID, REASON_CODE,
                    TERM_DATE, REVIEW_REQUIRED,
                    QUEUE_DATE, STATUS)
                   VALUES
                   (:HV-PROVIDER-ID,
                    :PTR-TERM-REASON-CODE,
                    :HV-TERM-DATE,
                    'Y',
                    CURRENT_TIMESTAMP,
                    'PD')
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

      * UPDATE PROVIDER_MASTER STATUS TO TERMINATED
           EXEC SQL
               UPDATE PROVIDER_MASTER
               SET STATUS = 'TM',
                   TERM_DATE = :HV-TERM-DATE,
                   TERM_REASON = :PTR-TERM-REASON-CODE,
                   ACCEPTING_NEW = 'N',
                   CONTRACT_TERM_DATE = :HV-TERM-DATE,
                   LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                   LAST_UPDATE_USER = :PTR-USER-ID,
                   LAST_UPDATE_PGM = :WS-PROGRAM-NAME
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
               GO TO 2300-EXIT
           END-IF

      * WRITE AUDIT TRAIL
           MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
           MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
           MOVE PTR-NPI TO AUD-NPI
           MOVE 'TRM' TO AUD-ACTION-TYPE
           MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
           MOVE PTR-USER-ID TO AUD-USER-ID
           MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
           MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
           MOVE 'STATUS' TO AUD-FIELD-NAME
           MOVE 'AC' TO AUD-BEFORE-VALUE
           MOVE 'TM' TO AUD-AFTER-VALUE
           MOVE HV-TERM-DATE TO AUD-EFF-DATE
           MOVE '00' TO AUD-RESULT-CODE
           STRING 'TERMINATED: ' PTR-TERM-REASON-CODE
               DELIMITED BY SIZE INTO AUD-RESULT-MSG
           END-STRING
           WRITE AUDIT-REC
           ADD 1 TO WS-AUDIT-CTR

      * WRITE TO MASTER OUTPUT FILE
           MOVE HV-PROVIDER-ID TO PMR-PROVIDER-ID
           MOVE PTR-NPI TO PMR-NPI
           MOVE 'TM' TO PMR-STATUS
           MOVE HV-TERM-DATE TO PMR-TERM-DATE
           MOVE 'T' TO PMR-ACTION-CODE
           WRITE PROVIDER-MASTER-REC
           ADD 1 TO WS-MASTER-WRITTEN-CTR

           ADD 1 TO WS-TERM-CTR.

       2300-EXIT.
           EXIT.

      ****************************************************************
      * 2400-PROVIDER-REACTIVATION
      * REACTIVATES A PREVIOUSLY TERMINATED PROVIDER. VERIFIES
      * THE PROVIDER WAS TERMINATED, CHECKS IF RE-CREDENTIALING
      * IS REQUIRED (ALWAYS IF TERMINATED > 180 DAYS), PERFORMS
      * SANCTION RE-CHECK AGAINST OIG/SAM, DETERMINES IF A NEW
      * CONTRACT IS REQUIRED, AND PERFORMS GAP ANALYSIS ON THE
      * PERIOD OF INACTIVITY.
      ****************************************************************
       2400-PROVIDER-REACTIVATION.

      * LOOK UP TERMINATED PROVIDER
           IF PTR-PROVIDER-ID NOT = SPACES
               MOVE PTR-PROVIDER-ID TO HV-PROVIDER-ID
           ELSE
               EXEC SQL
                   SELECT PROVIDER_ID
                   INTO :HV-PROVIDER-ID
                   FROM PROVIDER_MASTER
                   WHERE NPI = :PTR-NPI
                     AND STATUS = 'TM'
               END-EXEC
               IF SQLCODE NOT = 0
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'RA0001' TO ERR-CODE
                   MOVE 'NPI' TO ERR-FIELD-NAME
                   MOVE PTR-NPI TO ERR-FIELD-VALUE
                   MOVE 'NO TERMINATED PROVIDER FOUND FOR THIS NPI'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2400-EXIT
               END-IF
           END-IF

      * FETCH TERMINATION DETAILS
           EXEC SQL
               SELECT TERM_DATE, TERM_REASON,
                      CRED_DATE, CRED_STATUS
               INTO :HV-PREV-TERM-DATE, :HV-PREV-TERM-REASON,
                    :HV-CRED-DATE, :HV-CRED-STATUS
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
               GO TO 2400-EXIT
           END-IF

      * CHECK TERMINATION REASON - SOME REASONS PRECLUDE REACTIVATION
           EVALUATE HV-PREV-TERM-REASON
               WHEN 'I09'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'RA0010' TO ERR-CODE
                   MOVE 'TERM-REASON' TO ERR-FIELD-NAME
                   MOVE HV-PREV-TERM-REASON TO ERR-FIELD-VALUE
                   MOVE 'CANNOT REACTIVATE - OIG EXCLUDED PROVIDER'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2400-EXIT
               WHEN 'I10'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'RA0011' TO ERR-CODE
                   MOVE 'TERM-REASON' TO ERR-FIELD-NAME
                   MOVE HV-PREV-TERM-REASON TO ERR-FIELD-VALUE
                   MOVE 'CANNOT REACTIVATE - SAM EXCLUDED PROVIDER'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2400-EXIT
               WHEN 'I12'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'RA0012' TO ERR-CODE
                   MOVE 'TERM-REASON' TO ERR-FIELD-NAME
                   MOVE HV-PREV-TERM-REASON TO ERR-FIELD-VALUE
                   MOVE 'CANNOT REACTIVATE - FRAUD FINDING'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2400-EXIT
               WHEN 'I17'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'RA0013' TO ERR-CODE
                   MOVE 'TERM-REASON' TO ERR-FIELD-NAME
                   MOVE HV-PREV-TERM-REASON TO ERR-FIELD-VALUE
                   MOVE 'CANNOT REACTIVATE - CRIMINAL CONVICTION'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2400-EXIT
               WHEN 'I21'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'RA0014' TO ERR-CODE
                   MOVE 'TERM-REASON' TO ERR-FIELD-NAME
                   MOVE HV-PREV-TERM-REASON TO ERR-FIELD-VALUE
                   MOVE 'CANNOT REACTIVATE - CREDENTIAL FALSIFICATION'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2400-EXIT
               WHEN 'I22'
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'RA0015' TO ERR-CODE
                   MOVE 'TERM-REASON' TO ERR-FIELD-NAME
                   MOVE HV-PREV-TERM-REASON TO ERR-FIELD-VALUE
                   MOVE 'CANNOT REACTIVATE - SEXUAL MISCONDUCT'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 2400-EXIT
           END-EVALUATE

      * GAP ANALYSIS - CALCULATE DAYS SINCE TERMINATION
           EXEC SQL
               SELECT DATEDIFF(DAY, TERM_DATE, CURRENT_DATE)
               INTO :HV-GAP-MONTHS
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE = 0
      * IF TERMINATED MORE THAN 180 DAYS, FULL RE-CREDENTIALING
               IF HV-GAP-MONTHS > 180
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'RA0020' TO ERR-CODE
                   MOVE 'GAP-DAYS' TO ERR-FIELD-NAME
                   MOVE HV-GAP-MONTHS TO WS-SQLCODE-DISPLAY
                   MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
                   MOVE 'GAP > 180 DAYS - FULL RECREDENTIALING '
                       'REQUIRED BEFORE REACTIVATION'
                       TO ERR-MESSAGE
                   MOVE '2400-PROVIDER-REACTIVATION'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER

      * SET STATUS TO PENDING RECREDENTIALING
                   EXEC SQL
                       UPDATE PROVIDER_MASTER
                       SET STATUS = 'PR',
                           CRED_STATUS = 'PD',
                           TERM_DATE = NULL,
                           LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                           LAST_UPDATE_USER = :PTR-USER-ID,
                           LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                       WHERE PROVIDER_ID = :HV-PROVIDER-ID
                   END-EXEC
               ELSE
      * SHORT GAP - SANCTION RE-CHECK ONLY
                   PERFORM 3500-CHECK-SANCTIONS-EXCLUSIONS
                   IF WS-CRED-V-SAN-PASS
      * REACTIVATE WITH CURRENT CREDENTIALS
                       EXEC SQL
                           UPDATE PROVIDER_MASTER
                           SET STATUS = 'AC',
                               TERM_DATE = NULL,
                               TERM_REASON = NULL,
                               ACCEPTING_NEW = 'Y',
                               LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                               LAST_UPDATE_USER = :PTR-USER-ID,
                               LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                           WHERE PROVIDER_ID = :HV-PROVIDER-ID
                       END-EXEC
                   ELSE
                       MOVE 'E' TO ERR-SEVERITY
                       MOVE 'RA0030' TO ERR-CODE
                       MOVE 'SANCTION-CHECK' TO ERR-FIELD-NAME
                       MOVE 'FAIL' TO ERR-FIELD-VALUE
                       MOVE 'SANCTION CHECK FAILED - CANNOT REACTIVATE'
                           TO ERR-MESSAGE
                       MOVE '2400-PROVIDER-REACTIVATION'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                       GO TO 2400-EXIT
                   END-IF
               END-IF
           END-IF

      * CHECK IF NEW CONTRACT IS REQUIRED
           IF PTR-CONTRACT-ID NOT = SPACES
               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET CONTRACT_ID = :PTR-CONTRACT-ID,
                       CONTRACT_EFF_DATE = :PTR-CONTRACT-EFF-DATE,
                       CONTRACT_TERM_DATE = :PTR-CONTRACT-TERM-DATE,
                       FEE_SCHEDULE_ID = :PTR-FEE-SCHED-ID
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC
               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

      * WRITE AUDIT TRAIL
           MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
           MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
           MOVE PTR-NPI TO AUD-NPI
           MOVE 'REA' TO AUD-ACTION-TYPE
           MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
           MOVE PTR-USER-ID TO AUD-USER-ID
           MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
           MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
           MOVE 'STATUS' TO AUD-FIELD-NAME
           MOVE 'TM' TO AUD-BEFORE-VALUE
           MOVE 'AC' TO AUD-AFTER-VALUE
           MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
           MOVE '00' TO AUD-RESULT-CODE
           MOVE 'PROVIDER REACTIVATED' TO AUD-RESULT-MSG
           WRITE AUDIT-REC
           ADD 1 TO WS-AUDIT-CTR

           ADD 1 TO WS-REACT-CTR.

       2400-EXIT.
           EXIT.

      ****************************************************************
      * 3000-CREDENTIALING-PROCESS
      * FULL CREDENTIALING WORKFLOW. ORCHESTRATES ALL VERIFICATION
      * STEPS: EDUCATION, LICENSING, BOARD CERTIFICATION, MALPRACTICE,
      * WORK HISTORY, SANCTIONS/EXCLUSIONS. THEN CALCULATES THE
      * COMPOSITE CREDENTIAL SCORE AND DETERMINES APPROVAL STATUS.
      * SUPPORTS BOTH INITIAL CREDENTIALING AND RE-CREDENTIALING.
      * HANDLES DELEGATED CREDENTIALING FROM IPAS/MEDICAL GROUPS.
      ****************************************************************
       3000-CREDENTIALING-PROCESS.

      * INITIALIZE CREDENTIALING VERIFICATION STATUS
           INITIALIZE WS-CRED-VERIFICATION-STATUS
           INITIALIZE WS-CREDENTIAL-SCORING
           SET WS-CRED-FAILED TO TRUE

      * CHECK IF THIS IS DELEGATED CREDENTIALING
           SET WS-NOT-DELEGATED TO TRUE
           IF PTR-SOURCE-SYSTEM = 'DELG'
               SET WS-IS-DELEGATED TO TRUE
               PERFORM 3800-DELEGATED-CREDENTIALING
               GO TO 3000-EXIT
           END-IF

      * DETERMINE PROVIDER ID
           IF HV-PROVIDER-ID = SPACES
               IF PTR-PROVIDER-ID NOT = SPACES
                   MOVE PTR-PROVIDER-ID TO HV-PROVIDER-ID
               ELSE
                   EXEC SQL
                       SELECT PROVIDER_ID
                       INTO :HV-PROVIDER-ID
                       FROM PROVIDER_MASTER
                       WHERE NPI = :PTR-NPI
                         AND STATUS IN ('AC', 'PR', 'SU')
                   END-EXEC
                   IF SQLCODE NOT = 0
                       MOVE 'E' TO ERR-SEVERITY
                       MOVE 'CR0001' TO ERR-CODE
                       MOVE 'NPI' TO ERR-FIELD-NAME
                       MOVE PTR-NPI TO ERR-FIELD-VALUE
                       MOVE 'PROVIDER NOT FOUND FOR CREDENTIALING'
                           TO ERR-MESSAGE
                       MOVE '3000-CREDENTIALING-PROCESS'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                       GO TO 3000-EXIT
                   END-IF
               END-IF
           END-IF

      * CHECK FOR RECREDENTIALING CYCLE
           PERFORM 3700-RECREDENTIALING-CHECK

      * STEP 1: VERIFY EDUCATION
           PERFORM 3100-VERIFY-EDUCATION

      * STEP 2: VERIFY LICENSING
           PERFORM 3200-VERIFY-LICENSING

      * STEP 3: VERIFY MALPRACTICE
           PERFORM 3300-VERIFY-MALPRACTICE

      * STEP 4: VERIFY WORK HISTORY
           PERFORM 3400-VERIFY-WORK-HISTORY

      * STEP 5: CHECK SANCTIONS AND EXCLUSIONS
           PERFORM 3500-CHECK-SANCTIONS-EXCLUSIONS

      * STEP 6: CALCULATE COMPOSITE CREDENTIAL SCORE
           PERFORM 3600-CALCULATE-CREDENTIAL-SCORE

      * UPDATE CREDENTIALING RECORD IN DATABASE
           EXEC SQL
               SELECT NEXT_CRED_SEQ
               INTO :HV-NEXT-CRED-ID
               FROM SYSTEM_SEQUENCES
               WHERE SEQ_NAME = 'CRED_ID'
           END-EXEC

           IF SQLCODE NOT = 0
               ADD 1 TO WS-CRED-SEQ-CTR
               STRING 'CRD' WS-CRED-SEQ-CTR
                   DELIMITED BY SIZE INTO HV-NEXT-CRED-ID
               END-STRING
           ELSE
               EXEC SQL
                   UPDATE SYSTEM_SEQUENCES
                   SET NEXT_CRED_SEQ = NEXT_CRED_SEQ + 1
                   WHERE SEQ_NAME = 'CRED_ID'
               END-EXEC
           END-IF

           MOVE HV-NEXT-CRED-ID TO HV-CRED-ID
           MOVE HV-PROVIDER-ID TO HV-CRED-PROVIDER-ID
           MOVE 'IN' TO HV-CRED-TYPE
           MOVE WS-FORMATTED-DATE TO HV-CRED-INIT-DATE
           MOVE WS-FORMATTED-DATE TO HV-CRED-COMP-DATE
           MOVE WS-CRED-FINAL-STATUS TO HV-CRED-DECISION

           IF WS-CRED-V-ED-PASS
               MOVE 'Y' TO HV-CRED-ED-VERIFIED
           ELSE
               MOVE 'N' TO HV-CRED-ED-VERIFIED
           END-IF

           IF WS-CRED-V-LIC-PASS
               MOVE 'Y' TO HV-CRED-LIC-VERIFIED
           ELSE
               MOVE 'N' TO HV-CRED-LIC-VERIFIED
           END-IF

           IF WS-CRED-V-BRD-PASS
               MOVE 'Y' TO HV-CRED-BOARD-VERIFIED
           ELSE
               MOVE 'N' TO HV-CRED-BOARD-VERIFIED
           END-IF

           IF WS-CRED-V-MAL-PASS
               MOVE 'Y' TO HV-CRED-MAL-VERIFIED
           ELSE
               MOVE 'N' TO HV-CRED-MAL-VERIFIED
           END-IF

           IF WS-CRED-V-WRK-PASS
               MOVE 'Y' TO HV-CRED-WORK-VERIFIED
           ELSE
               MOVE 'N' TO HV-CRED-WORK-VERIFIED
           END-IF

           IF WS-CRED-V-REF-PASS
               MOVE 'Y' TO HV-CRED-REF-VERIFIED
           ELSE
               MOVE 'N' TO HV-CRED-REF-VERIFIED
           END-IF

           IF WS-CRED-V-SAN-PASS
               MOVE 'Y' TO HV-CRED-SANC-VERIFIED
           ELSE
               MOVE 'N' TO HV-CRED-SANC-VERIFIED
           END-IF

           MOVE WS-CRED-TOTAL-SCORE TO HV-CRED-SCORE-VAL
           MOVE 'N' TO HV-CRED-DELEGATED-SW

           EXEC SQL
               INSERT INTO CREDENTIALING_HISTORY
               (CRED_ID, PROVIDER_ID, CRED_TYPE,
                INITIATION_DATE, COMPLETION_DATE,
                DECISION, COMMITTEE_DATE,
                EDUCATION_VERIFIED, LICENSE_VERIFIED,
                BOARD_VERIFIED, MALPRACTICE_VERIFIED,
                WORK_VERIFIED, REFERENCE_VERIFIED,
                SANCTION_VERIFIED, SITE_VERIFIED,
                CRED_SCORE, DELEGATED_IND,
                CREATED_DATE, CREATED_BY)
               VALUES
               (:HV-CRED-ID, :HV-CRED-PROVIDER-ID,
                :HV-CRED-TYPE,
                :HV-CRED-INIT-DATE, :HV-CRED-COMP-DATE,
                :HV-CRED-DECISION, :WS-FORMATTED-DATE,
                :HV-CRED-ED-VERIFIED, :HV-CRED-LIC-VERIFIED,
                :HV-CRED-BOARD-VERIFIED, :HV-CRED-MAL-VERIFIED,
                :HV-CRED-WORK-VERIFIED, :HV-CRED-REF-VERIFIED,
                :HV-CRED-SANC-VERIFIED,
                :HV-CRED-V-SITE-STATUS,
                :HV-CRED-SCORE-VAL, :HV-CRED-DELEGATED-SW,
                CURRENT_TIMESTAMP, :PTR-USER-ID)
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF

      * UPDATE PROVIDER_MASTER WITH CREDENTIALING RESULTS
      * CALCULATE RECREDENTIALING DUE DATE (3 YEARS)
           EXEC SQL
               UPDATE PROVIDER_MASTER
               SET CRED_STATUS = :WS-CRED-FINAL-STATUS,
                   CRED_DATE = :WS-FORMATTED-DATE,
                   RECRED_DUE_DATE =
                       DATEADD(YEAR, 3, CURRENT_DATE),
                   CRED_SCORE = :WS-CRED-TOTAL-SCORE,
                   LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                   LAST_UPDATE_USER = :PTR-USER-ID,
                   LAST_UPDATE_PGM = :WS-PROGRAM-NAME
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF

           ADD 1 TO WS-CRED-CTR

           EVALUATE TRUE
               WHEN WS-CRED-FULL-APPROVAL
                   ADD 1 TO WS-CRED-PASSED-CTR
               WHEN WS-CRED-CONDITIONAL
                   ADD 1 TO WS-CRED-CONDITIONAL-CTR
               WHEN WS-CRED-DENIED
                   ADD 1 TO WS-CRED-FAILED-CTR
           END-EVALUATE.

       3000-EXIT.
           EXIT.

      ****************************************************************
      * 3100-VERIFY-EDUCATION
      * VERIFIES PROVIDER EDUCATION CREDENTIALS INCLUDING MEDICAL
      * SCHOOL GRADUATION, RESIDENCY COMPLETION, FELLOWSHIP (IF
      * APPLICABLE), AND BOARD CERTIFICATION STATUS VIA ABMS.
      ****************************************************************
       3100-VERIFY-EDUCATION.

           MOVE 'PD' TO WS-CRED-V-ED-STATUS
           MOVE SPACES TO WS-CRED-V-ED-DATE
           MOVE ZERO TO WS-CRED-EDUCATION-SCORE

      * FOR ORGANIZATIONS (TYPE 2), EDUCATION NOT APPLICABLE
           IF PTR-ENTITY-TYPE = '2'
               MOVE 'NA' TO WS-CRED-V-ED-STATUS
               MOVE WS-CRED-WEIGHT-EDUCATION
                   TO WS-CRED-EDUCATION-SCORE
               GO TO 3100-EXIT
           END-IF

      * VERIFY MEDICAL SCHOOL
           IF PTR-MED-SCHOOL NOT = SPACES
               EXEC SQL
                   SELECT COUNT(*)
                   INTO :HV-ROW-COUNT
                   FROM MEDICAL_SCHOOL_REGISTRY
                   WHERE UPPER(SCHOOL_NAME) =
                         UPPER(:PTR-MED-SCHOOL)
                     AND ACCREDITATION_STATUS = 'AC'
               END-EXEC

               IF SQLCODE = 0 AND HV-ROW-COUNT > 0
                   ADD 6 TO WS-CRED-EDUCATION-SCORE
                   MOVE 'MEDICAL SCHOOL REGISTRY'
                       TO WS-CRED-V-ED-SOURCE
               ELSE
      * SCHOOL NOT IN REGISTRY - MAY BE INTERNATIONAL
                   EXEC SQL
                       SELECT COUNT(*)
                       INTO :HV-ROW-COUNT
                       FROM INTERNATIONAL_MEDICAL_GRAD
                       WHERE UPPER(SCHOOL_NAME) =
                             UPPER(:PTR-MED-SCHOOL)
                         AND ECFMG_CERTIFIED = 'Y'
                   END-EXEC

                   IF SQLCODE = 0 AND HV-ROW-COUNT > 0
                       ADD 5 TO WS-CRED-EDUCATION-SCORE
                       MOVE 'ECFMG CERTIFICATION'
                           TO WS-CRED-V-ED-SOURCE
                   ELSE
                       ADD 2 TO WS-CRED-EDUCATION-SCORE
                       MOVE 'UNVERIFIED - MANUAL REVIEW NEEDED'
                           TO WS-CRED-V-ED-NOTES
                   END-IF
               END-IF
           ELSE
               MOVE 'MEDICAL SCHOOL NOT PROVIDED'
                   TO WS-CRED-V-ED-NOTES
           END-IF

      * VERIFY RESIDENCY COMPLETION
           IF PTR-RESIDENCY-INST NOT = SPACES
               EXEC SQL
                   SELECT COUNT(*)
                   INTO :HV-ROW-COUNT
                   FROM RESIDENCY_VERIFICATION
                   WHERE PROVIDER_NPI = :PTR-NPI
                     AND UPPER(INSTITUTION) =
                         UPPER(:PTR-RESIDENCY-INST)
                     AND VERIFICATION_STATUS = 'VF'
               END-EXEC

               IF SQLCODE = 0 AND HV-ROW-COUNT > 0
                   ADD 5 TO WS-CRED-EDUCATION-SCORE
               ELSE
                   ADD 2 TO WS-CRED-EDUCATION-SCORE
               END-IF
           END-IF

      * VERIFY FELLOWSHIP IF APPLICABLE
           IF PTR-FELLOWSHIP-INST NOT = SPACES
               ADD 3 TO WS-CRED-EDUCATION-SCORE
           END-IF

      * VERIFY BOARD CERTIFICATION VIA ABMS
           IF PTR-BOARD-CERT-STATUS = 'Y'
               EXEC SQL
                   SELECT COUNT(*)
                   INTO :HV-ROW-COUNT
                   FROM ABMS_BOARD_CERTIFICATION
                   WHERE PROVIDER_NPI = :PTR-NPI
                     AND CERTIFICATION_STATUS = 'AC'
                     AND (EXPIRATION_DATE IS NULL
                          OR EXPIRATION_DATE >= CURRENT_DATE)
               END-EXEC

               IF SQLCODE = 0 AND HV-ROW-COUNT > 0
                   ADD 6 TO WS-CRED-EDUCATION-SCORE
                   MOVE WS-FORMATTED-DATE TO WS-CRED-V-BRD-DATE
                   MOVE PTR-BOARD-EXP-DATE TO WS-CRED-V-BRD-EXP
                   MOVE 'PS' TO WS-CRED-V-BRD-STATUS
                   MOVE 'ABMS REGISTRY' TO WS-CRED-V-BRD-SOURCE
               ELSE
                   ADD 2 TO WS-CRED-EDUCATION-SCORE
                   MOVE 'FL' TO WS-CRED-V-BRD-STATUS
               END-IF
           ELSE
      * BOARD CERTIFICATION NOT REQUIRED FOR ALL SPECIALTIES
               PERFORM VARYING WS-TAX-IDX FROM 1 BY 1
                   UNTIL WS-TAX-IDX > WS-TAXONOMY-COUNT
                   IF WS-TAX-CODE(WS-TAX-IDX) = PTR-TAXONOMY-CODE
                       IF WS-TAX-REQUIRES-BOARD(WS-TAX-IDX) = 'N'
                           MOVE 'NA' TO WS-CRED-V-BRD-STATUS
                           ADD 4 TO WS-CRED-EDUCATION-SCORE
                       ELSE
                           MOVE 'FL' TO WS-CRED-V-BRD-STATUS
                       END-IF
                       SET WS-TAX-IDX TO WS-TAXONOMY-COUNT
                   END-IF
               END-PERFORM
           END-IF

      * NORMALIZE EDUCATION SCORE TO WEIGHT
           IF WS-CRED-EDUCATION-SCORE > WS-CRED-WEIGHT-EDUCATION
               MOVE WS-CRED-WEIGHT-EDUCATION
                   TO WS-CRED-EDUCATION-SCORE
           END-IF

      * DETERMINE OVERALL EDUCATION VERIFICATION STATUS
           IF WS-CRED-EDUCATION-SCORE >= 14
               MOVE 'PS' TO WS-CRED-V-ED-STATUS
           ELSE
               IF WS-CRED-EDUCATION-SCORE >= 8
                   MOVE 'PS' TO WS-CRED-V-ED-STATUS
                   MOVE 'PARTIAL VERIFICATION - CONDITIONAL'
                       TO WS-CRED-V-ED-NOTES
               ELSE
                   MOVE 'FL' TO WS-CRED-V-ED-STATUS
               END-IF
           END-IF

           MOVE WS-FORMATTED-DATE TO WS-CRED-V-ED-DATE.

       3100-EXIT.
           EXIT.

      ****************************************************************
      * 3200-VERIFY-LICENSING
      * VERIFIES STATE MEDICAL LICENSE INCLUDING EXPIRATION,
      * RESTRICTIONS, MULTI-STATE TRACKING, DISCIPLINARY ACTIONS,
      * DEA SCHEDULE VERIFICATION, AND STATE CONTROLLED SUBSTANCE
      * LICENSE REQUIREMENTS.
      ****************************************************************
       3200-VERIFY-LICENSING.

           MOVE 'PD' TO WS-CRED-V-LIC-STATUS
           MOVE ZERO TO WS-CRED-LICENSE-SCORE

      * FOR ORGANIZATIONS, LICENSE VERIFICATION IS DIFFERENT
           IF PTR-ENTITY-TYPE = '2'
               MOVE 'NA' TO WS-CRED-V-LIC-STATUS
               MOVE WS-CRED-WEIGHT-LICENSE
                   TO WS-CRED-LICENSE-SCORE
               GO TO 3200-EXIT
           END-IF

      * VERIFY PRIMARY STATE LICENSE
           IF PTR-LICENSE-NUMBER = SPACES
               MOVE 'FL' TO WS-CRED-V-LIC-STATUS
               MOVE 'NO LICENSE NUMBER PROVIDED'
                   TO WS-CRED-V-LIC-NOTES
               GO TO 3200-EXIT
           END-IF

      * QUERY LICENSE VERIFICATION DATABASE
           EXEC SQL
               SELECT LICENSE_STATUS, LICENSE_ACTIONS,
                      LICENSE_RESTRICTIONS,
                      VERIFICATION_DATE
               INTO :WS-CRED-V-LIC-ACTION,
                    :WS-WORK-STRING-1,
                    :WS-WORK-STRING-2,
                    :WS-CRED-V-LIC-DATE
               FROM LICENSE_VERIFICATION
               WHERE LICENSE_NUMBER = :PTR-LICENSE-NUMBER
                 AND LICENSE_STATE = :PTR-LICENSE-STATE
               ORDER BY VERIFICATION_DATE DESC
           END-EXEC

           IF SQLCODE = 0
      * LICENSE FOUND - CHECK STATUS
               EVALUATE WS-CRED-V-LIC-ACTION
                   WHEN 'CL'
                       ADD 15 TO WS-CRED-LICENSE-SCORE
                       MOVE 'STATE BOARD VERIFICATION'
                           TO WS-CRED-V-LIC-SOURCE
                   WHEN 'PR'
                       ADD 8 TO WS-CRED-LICENSE-SCORE
                       MOVE 'LICENSE ON PROBATION'
                           TO WS-CRED-V-LIC-NOTES
                   WHEN 'RS'
                       ADD 5 TO WS-CRED-LICENSE-SCORE
                       MOVE 'LICENSE HAS RESTRICTIONS'
                           TO WS-CRED-V-LIC-NOTES
                   WHEN 'SU'
                       MOVE ZERO TO WS-CRED-LICENSE-SCORE
                       MOVE 'LICENSE IS SUSPENDED'
                           TO WS-CRED-V-LIC-NOTES
                   WHEN 'RV'
                       MOVE ZERO TO WS-CRED-LICENSE-SCORE
                       MOVE 'LICENSE HAS BEEN REVOKED'
                           TO WS-CRED-V-LIC-NOTES
                   WHEN 'SR'
                       MOVE ZERO TO WS-CRED-LICENSE-SCORE
                       MOVE 'LICENSE WAS SURRENDERED'
                           TO WS-CRED-V-LIC-NOTES
               END-EVALUATE
           ELSE
               IF SQLCODE = 100
      * NO VERIFICATION RECORD - USE SUBMITTED DATA
                   IF PTR-LICENSE-EXP-DATE >= WS-FORMATTED-DATE
                       ADD 10 TO WS-CRED-LICENSE-SCORE
                       MOVE 'PROVIDER ATTESTATION - PENDING PRIMARY'
                           TO WS-CRED-V-LIC-SOURCE
                   ELSE
                       MOVE ZERO TO WS-CRED-LICENSE-SCORE
                       MOVE 'LICENSE EXPIRED PER SUBMITTED DATE'
                           TO WS-CRED-V-LIC-NOTES
                   END-IF
               ELSE
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

      * CHECK LICENSE EXPIRATION DATE
           IF PTR-LICENSE-EXP-DATE NOT = SPACES
               IF PTR-LICENSE-EXP-DATE < WS-FORMATTED-DATE
                   MOVE ZERO TO WS-CRED-LICENSE-SCORE
                   MOVE 'FL' TO WS-CRED-V-LIC-STATUS
                   MOVE 'LICENSE HAS EXPIRED'
                       TO WS-CRED-V-LIC-NOTES
                   GO TO 3200-EXIT
               END-IF
           END-IF

      * CHECK MULTI-STATE LICENSE (INTERSTATE COMPACT)
           PERFORM VARYING WS-ST-IDX FROM 1 BY 1
               UNTIL WS-ST-IDX > WS-STATE-COUNT
               IF WS-ST-CODE(WS-ST-IDX) = PTR-LICENSE-STATE
                   IF WS-ST-COMPACT-MEMBER(WS-ST-IDX) = 'Y'
                       ADD 2 TO WS-CRED-LICENSE-SCORE
                   END-IF
               END-IF
           END-PERFORM

      * VERIFY DEA REGISTRATION AND SCHEDULES
           IF PTR-DEA-NUMBER NOT = SPACES
               MOVE PTR-DEA-NUMBER TO WS-DEA-WORK
               PERFORM 2130-VALIDATE-DEA
               IF WS-DEA-VALID
                   ADD 5 TO WS-CRED-LICENSE-SCORE

      * VERIFY DEA SCHEDULES
                   IF PTR-DEA-SCHEDULES NOT = SPACES
                       IF PTR-DEA-SCHEDULES(1:1) = 'Y'
                           MOVE 'Y' TO WS-DEA-SCHED-II
                       END-IF
                       IF PTR-DEA-SCHEDULES(2:1) = 'Y'
                           MOVE 'Y' TO WS-DEA-SCHED-III
                       END-IF
                       IF PTR-DEA-SCHEDULES(3:1) = 'Y'
                           MOVE 'Y' TO WS-DEA-SCHED-IV
                       END-IF
                       IF PTR-DEA-SCHEDULES(4:1) = 'Y'
                           MOVE 'Y' TO WS-DEA-SCHED-V
                       END-IF
                   END-IF
               ELSE
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'CR0210' TO ERR-CODE
                   MOVE 'DEA-NUMBER' TO ERR-FIELD-NAME
                   MOVE PTR-DEA-NUMBER TO ERR-FIELD-VALUE
                   MOVE 'DEA VALIDATION FAILED IN CREDENTIALING'
                       TO ERR-MESSAGE
                   MOVE '3200-VERIFY-LICENSING'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           ELSE
      * CHECK IF DEA IS REQUIRED FOR THIS STATE
               PERFORM VARYING WS-ST-IDX FROM 1 BY 1
                   UNTIL WS-ST-IDX > WS-STATE-COUNT
                   IF WS-ST-CODE(WS-ST-IDX) = PTR-LICENSE-STATE
                       IF WS-ST-DEA-REQUIRED(WS-ST-IDX) = 'Y'
                           MOVE 'W' TO ERR-SEVERITY
                           MOVE 'CR0211' TO ERR-CODE
                           MOVE 'DEA-REQUIRED' TO ERR-FIELD-NAME
                           MOVE PTR-LICENSE-STATE TO ERR-FIELD-VALUE
                           MOVE 'DEA REGISTRATION REQUIRED FOR STATE'
                               TO ERR-MESSAGE
                           MOVE '3200-VERIFY-LICENSING'
                               TO ERR-PARAGRAPH-NAME
                           PERFORM 8000-ERROR-HANDLER
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

      * CHECK STATE CONTROLLED SUBSTANCE REGISTRATION
           PERFORM VARYING WS-ST-IDX FROM 1 BY 1
               UNTIL WS-ST-IDX > WS-STATE-COUNT
               IF WS-ST-CODE(WS-ST-IDX) = PTR-LICENSE-STATE
                   IF WS-ST-CSR-REQUIRED(WS-ST-IDX) = 'Y'
                       EXEC SQL
                           SELECT COUNT(*)
                           INTO :HV-ROW-COUNT
                           FROM STATE_CONTROLLED_SUBSTANCE_REG
                           WHERE PROVIDER_NPI = :PTR-NPI
                             AND STATE_CODE = :PTR-LICENSE-STATE
                             AND STATUS = 'AC'
                             AND EXPIRATION_DATE >= CURRENT_DATE
                       END-EXEC

                       IF SQLCODE = 0 AND HV-ROW-COUNT > 0
                           ADD 3 TO WS-CRED-LICENSE-SCORE
                       ELSE
                           MOVE 'W' TO ERR-SEVERITY
                           MOVE 'CR0212' TO ERR-CODE
                           MOVE 'CSR' TO ERR-FIELD-NAME
                           MOVE PTR-LICENSE-STATE TO ERR-FIELD-VALUE
                           MOVE 'STATE CONTROLLED SUBSTANCE '
                               'REGISTRATION NOT FOUND'
                               TO ERR-MESSAGE
                           MOVE '3200-VERIFY-LICENSING'
                               TO ERR-PARAGRAPH-NAME
                           PERFORM 8000-ERROR-HANDLER
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

      * NORMALIZE LICENSE SCORE
           IF WS-CRED-LICENSE-SCORE > WS-CRED-WEIGHT-LICENSE
               MOVE WS-CRED-WEIGHT-LICENSE
                   TO WS-CRED-LICENSE-SCORE
           END-IF

      * DETERMINE OVERALL LICENSE STATUS
           IF WS-CRED-LICENSE-SCORE >= 15
               MOVE 'PS' TO WS-CRED-V-LIC-STATUS
           ELSE
               IF WS-CRED-LICENSE-SCORE >= 8
                   MOVE 'PS' TO WS-CRED-V-LIC-STATUS
               ELSE
                   MOVE 'FL' TO WS-CRED-V-LIC-STATUS
               END-IF
           END-IF

           MOVE WS-FORMATTED-DATE TO WS-CRED-V-LIC-DATE.

       3200-EXIT.
           EXIT.

      ****************************************************************
      * 3300-VERIFY-MALPRACTICE
      * VERIFIES MALPRACTICE INSURANCE COVERAGE, MINIMUM LIMITS
      * ($1M PER OCCURRENCE / $3M AGGREGATE), REVIEWS CLAIMS HISTORY
      * FOR 5-YEAR LOOKBACK PERIOD, AND TRACKS NPDB QUERIES.
      ****************************************************************
       3300-VERIFY-MALPRACTICE.

           MOVE 'PD' TO WS-CRED-V-MAL-STATUS
           MOVE ZERO TO WS-CRED-MALPRACT-SCORE

      * ORGANIZATIONS MAY SELF-INSURE
           IF PTR-ENTITY-TYPE = '2'
               MOVE 'NA' TO WS-CRED-V-MAL-STATUS
               MOVE WS-CRED-WEIGHT-MALPRACT
                   TO WS-CRED-MALPRACT-SCORE
               GO TO 3300-EXIT
           END-IF

      * CHECK FOR MALPRACTICE INSURANCE
           IF PTR-MALPRACTICE-CARRIER = SPACES
               MOVE 'FL' TO WS-CRED-V-MAL-STATUS
               MOVE 'NO MALPRACTICE INSURANCE ON FILE'
                   TO WS-CRED-V-ED-NOTES
               GO TO 3300-EXIT
           END-IF

      * VERIFY COVERAGE AMOUNTS MEET MINIMUMS
      * MINIMUM: $1,000,000 PER OCCURRENCE / $3,000,000 AGGREGATE
           MOVE PTR-MALPRACTICE-PER-OCC TO WS-CRED-V-MAL-PER-OCC
           MOVE PTR-MALPRACTICE-AGGREG  TO WS-CRED-V-MAL-AGGREG

           IF WS-CRED-V-MAL-PER-OCC >= 1000000
               ADD 5 TO WS-CRED-MALPRACT-SCORE
           ELSE
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'CR0301' TO ERR-CODE
               MOVE 'MAL-PER-OCC' TO ERR-FIELD-NAME
               MOVE WS-CRED-V-MAL-PER-OCC TO WS-SQLCODE-DISPLAY
               MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
               MOVE 'MALPRACTICE PER-OCCURRENCE BELOW $1M MINIMUM'
                   TO ERR-MESSAGE
               MOVE '3300-VERIFY-MALPRACTICE' TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

           IF WS-CRED-V-MAL-AGGREG >= 3000000
               ADD 5 TO WS-CRED-MALPRACT-SCORE
           ELSE
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'CR0302' TO ERR-CODE
               MOVE 'MAL-AGGREGATE' TO ERR-FIELD-NAME
               MOVE WS-CRED-V-MAL-AGGREG TO WS-SQLCODE-DISPLAY
               MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
               MOVE 'MALPRACTICE AGGREGATE BELOW $3M MINIMUM'
                   TO ERR-MESSAGE
               MOVE '3300-VERIFY-MALPRACTICE' TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * CHECK MALPRACTICE EXPIRATION
           IF PTR-MALPRACTICE-EXP NOT = SPACES
               IF PTR-MALPRACTICE-EXP < WS-FORMATTED-DATE
                   MOVE 'FL' TO WS-CRED-V-MAL-STATUS
                   GO TO 3300-EXIT
               END-IF
           END-IF

      * REVIEW CLAIMS HISTORY - 5 YEAR LOOKBACK
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-MALPRACTICE-CLAIMS
               FROM MALPRACTICE_CLAIMS_HISTORY
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND CLAIM_DATE >=
                     DATEADD(YEAR, -5, CURRENT_DATE)
                 AND DISPOSITION IN ('SE', 'JV', 'AR')
           END-EXEC

           IF SQLCODE = 0
               MOVE HV-MALPRACTICE-CLAIMS TO WS-CRED-V-MAL-CLAIMS
               IF HV-MALPRACTICE-CLAIMS = 0
                   ADD 3 TO WS-CRED-MALPRACT-SCORE
               ELSE
                   IF HV-MALPRACTICE-CLAIMS <= 2
                       ADD 1 TO WS-CRED-MALPRACT-SCORE
                   ELSE
                       MOVE 'W' TO ERR-SEVERITY
                       MOVE 'CR0310' TO ERR-CODE
                       MOVE 'MAL-CLAIMS' TO ERR-FIELD-NAME
                       MOVE HV-MALPRACTICE-CLAIMS
                           TO WS-SQLCODE-DISPLAY
                       MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
                       MOVE 'EXCESSIVE MALPRACTICE CLAIMS IN 5YR '
                           'LOOKBACK - COMMITTEE REVIEW REQUIRED'
                           TO ERR-MESSAGE
                       MOVE '3300-VERIFY-MALPRACTICE'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                   END-IF
               END-IF
           END-IF

      * TRACK NPDB QUERY
           EXEC SQL
               SELECT QUERY_RESULT, QUERY_DATE
               INTO :HV-CRED-NPDB-RESULT, :HV-CRED-NPDB-DATE
               FROM NPDB_QUERY_LOG
               WHERE PROVIDER_NPI = :PTR-NPI
               ORDER BY QUERY_DATE DESC
           END-EXEC

           IF SQLCODE = 0
               MOVE 'Y' TO HV-CRED-NPDB-QUERIED
               IF HV-CRED-NPDB-RESULT = 'CL'
                   ADD 2 TO WS-CRED-MALPRACT-SCORE
                   MOVE 'CL' TO WS-CRED-V-MAL-NPDB
               ELSE
                   MOVE HV-CRED-NPDB-RESULT TO WS-CRED-V-MAL-NPDB
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'CR0320' TO ERR-CODE
                   MOVE 'NPDB-RESULT' TO ERR-FIELD-NAME
                   MOVE HV-CRED-NPDB-RESULT TO ERR-FIELD-VALUE
                   MOVE 'NPDB REPORTS ADVERSE ACTION - REVIEW REQUIRED'
                       TO ERR-MESSAGE
                   MOVE '3300-VERIFY-MALPRACTICE'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           ELSE
               MOVE 'N' TO HV-CRED-NPDB-QUERIED
               MOVE 'NQ' TO WS-CRED-V-MAL-NPDB
           END-IF

      * NORMALIZE MALPRACTICE SCORE
           IF WS-CRED-MALPRACT-SCORE > WS-CRED-WEIGHT-MALPRACT
               MOVE WS-CRED-WEIGHT-MALPRACT
                   TO WS-CRED-MALPRACT-SCORE
           END-IF

      * DETERMINE STATUS
           IF WS-CRED-MALPRACT-SCORE >= 10
               MOVE 'PS' TO WS-CRED-V-MAL-STATUS
           ELSE
               IF WS-CRED-MALPRACT-SCORE >= 5
                   MOVE 'PS' TO WS-CRED-V-MAL-STATUS
               ELSE
                   MOVE 'FL' TO WS-CRED-V-MAL-STATUS
               END-IF
           END-IF

           MOVE WS-FORMATTED-DATE TO WS-CRED-V-MAL-DATE.

       3300-EXIT.
           EXIT.

      ****************************************************************
      * 3400-VERIFY-WORK-HISTORY
      * VERIFIES EMPLOYMENT HISTORY WITH 5-YEAR MINIMUM LOOKBACK,
      * IDENTIFIES GAPS > 6 MONTHS REQUIRING EXPLANATION, VERIFIES
      * HOSPITAL PRIVILEGES, AND CHECKS CLINICAL COMPETENCY.
      ****************************************************************
       3400-VERIFY-WORK-HISTORY.

           MOVE 'PD' TO WS-CRED-V-WRK-STATUS
           MOVE ZERO TO WS-CRED-WORKHIST-SCORE
           MOVE ZERO TO WS-CRED-V-WRK-GAPS

      * QUERY WORK HISTORY RECORDS
           EXEC SQL
               SELECT COUNT(*),
                      SUM(CASE WHEN GAP_MONTHS > 6 THEN 1 ELSE 0 END)
               INTO :HV-ROW-COUNT, :HV-GAP-MONTHS
               FROM PROVIDER_WORK_HISTORY
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND START_DATE >=
                     DATEADD(YEAR, -5, CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               IF HV-ROW-COUNT > 0
                   ADD 5 TO WS-CRED-WORKHIST-SCORE

      * CHECK FOR GAPS > 6 MONTHS
                   IF HV-GAP-MONTHS > 0
                       MOVE HV-GAP-MONTHS TO WS-CRED-V-WRK-GAPS

      * CHECK IF GAPS HAVE EXPLANATIONS
                       EXEC SQL
                           SELECT COUNT(*)
                           INTO :HV-ROW-COUNT
                           FROM PROVIDER_WORK_HISTORY
                           WHERE PROVIDER_NPI = :PTR-NPI
                             AND GAP_MONTHS > 6
                             AND GAP_EXPLANATION IS NOT NULL
                             AND GAP_EXPLANATION <> ''
                       END-EXEC

                       IF SQLCODE = 0
                           IF HV-ROW-COUNT >= HV-GAP-MONTHS
                               ADD 3 TO WS-CRED-WORKHIST-SCORE
                               MOVE 'Y' TO WS-CRED-V-WRK-EXPLAIN
                           ELSE
                               MOVE 'N' TO WS-CRED-V-WRK-EXPLAIN
                               MOVE 'W' TO ERR-SEVERITY
                               MOVE 'CR0401' TO ERR-CODE
                               MOVE 'WORK-GAPS' TO ERR-FIELD-NAME
                               MOVE HV-GAP-MONTHS
                                   TO WS-SQLCODE-DISPLAY
                               MOVE WS-SQLCODE-DISPLAY
                                   TO ERR-FIELD-VALUE
                               MOVE 'WORK HISTORY GAPS > 6 MONTHS '
                                   'WITHOUT EXPLANATION'
                                   TO ERR-MESSAGE
                               MOVE '3400-VERIFY-WORK-HISTORY'
                                   TO ERR-PARAGRAPH-NAME
                               PERFORM 8000-ERROR-HANDLER
                           END-IF
                       END-IF
                   ELSE
                       ADD 3 TO WS-CRED-WORKHIST-SCORE
                   END-IF
               ELSE
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'CR0400' TO ERR-CODE
                   MOVE 'WORK-HISTORY' TO ERR-FIELD-NAME
                   MOVE SPACES TO ERR-FIELD-VALUE
                   MOVE 'NO WORK HISTORY RECORDS FOUND FOR 5YR PERIOD'
                       TO ERR-MESSAGE
                   MOVE '3400-VERIFY-WORK-HISTORY'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           END-IF

      * VERIFY HOSPITAL PRIVILEGES
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM HOSPITAL_PRIVILEGES
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND PRIVILEGE_STATUS = 'AC'
                 AND (EXPIRATION_DATE IS NULL
                      OR EXPIRATION_DATE >= CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0 AND HV-ROW-COUNT > 0
               ADD 2 TO WS-CRED-WORKHIST-SCORE
           END-IF

      * NORMALIZE WORK HISTORY SCORE
           IF WS-CRED-WORKHIST-SCORE > WS-CRED-WEIGHT-WORKHIST
               MOVE WS-CRED-WEIGHT-WORKHIST
                   TO WS-CRED-WORKHIST-SCORE
           END-IF

      * SET STATUS
           IF WS-CRED-WORKHIST-SCORE >= 7
               MOVE 'PS' TO WS-CRED-V-WRK-STATUS
           ELSE
               IF WS-CRED-WORKHIST-SCORE >= 4
                   MOVE 'PS' TO WS-CRED-V-WRK-STATUS
               ELSE
                   MOVE 'FL' TO WS-CRED-V-WRK-STATUS
               END-IF
           END-IF

           MOVE WS-FORMATTED-DATE TO WS-CRED-V-WRK-DATE.

       3400-EXIT.
           EXIT.

      ****************************************************************
      * 3500-CHECK-SANCTIONS-EXCLUSIONS
      * COMPREHENSIVE SANCTIONS SCREENING: OIG LEIE, SAM, STATE
      * MEDICAID EXCLUSION LIST, STATE LICENSING BOARD ACTIONS,
      * MEDICARE OPT-OUT CHECK, NATIONAL SEX OFFENDER REGISTRY
      * (FOR PEDIATRIC PROVIDERS), AND DEA SANCTION CHECK.
      ****************************************************************
       3500-CHECK-SANCTIONS-EXCLUSIONS.

           MOVE 'PD' TO WS-CRED-V-SAN-STATUS
           MOVE ZERO TO WS-CRED-SANCTION-SCORE
           MOVE 'N' TO WS-CRED-V-SAN-OIG
           MOVE 'N' TO WS-CRED-V-SAN-SAM
           MOVE 'N' TO WS-CRED-V-SAN-STATE
           MOVE 'N' TO WS-CRED-V-SAN-NSOPW
           MOVE 'N' TO WS-CRED-V-SAN-DEA
           MOVE 'N' TO WS-CRED-V-SAN-MCRE-OO

      * 1. OIG LEIE CHECK
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM OIG_EXCLUSION_MATCH
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND MATCH_STATUS = 'CF'
                 AND (REINSTATEMENT_DATE IS NULL
                      OR REINSTATEMENT_DATE > CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               IF HV-ROW-COUNT > 0
                   MOVE 'Y' TO WS-CRED-V-SAN-OIG
                   MOVE 'FL' TO WS-CRED-V-SAN-STATUS
                   MOVE 'C' TO ERR-SEVERITY
                   MOVE 'CR0501' TO ERR-CODE
                   MOVE 'OIG-EXCLUSION' TO ERR-FIELD-NAME
                   MOVE PTR-NPI TO ERR-FIELD-VALUE
                   MOVE 'PROVIDER IS ON OIG EXCLUSION LIST - '
                       'IMMEDIATE ACTION REQUIRED'
                       TO ERR-MESSAGE
                   MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 3500-EXIT
               ELSE
                   MOVE 'N' TO WS-CRED-V-SAN-OIG
               END-IF
           END-IF

      * 2. SAM EXCLUSION CHECK
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM SAM_EXCLUSION_MATCH
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND MATCH_STATUS = 'CF'
                 AND (TERMINATION_DATE IS NULL
                      OR TERMINATION_DATE > CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               IF HV-ROW-COUNT > 0
                   MOVE 'Y' TO WS-CRED-V-SAN-SAM
                   MOVE 'FL' TO WS-CRED-V-SAN-STATUS
                   MOVE 'C' TO ERR-SEVERITY
                   MOVE 'CR0502' TO ERR-CODE
                   MOVE 'SAM-EXCLUSION' TO ERR-FIELD-NAME
                   MOVE PTR-NPI TO ERR-FIELD-VALUE
                   MOVE 'PROVIDER IS ON SAM EXCLUSION LIST - '
                       'IMMEDIATE ACTION REQUIRED'
                       TO ERR-MESSAGE
                   MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 3500-EXIT
               ELSE
                   MOVE 'N' TO WS-CRED-V-SAN-SAM
               END-IF
           END-IF

      * 3. STATE MEDICAID EXCLUSION CHECK
           EXEC SQL
               SELECT EXCLUSION_STATUS
               INTO :HV-STATE-EXCL-RESULT
               FROM STATE_MEDICAID_EXCLUSION
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND STATE_CODE = :PTR-LICENSE-STATE
                 AND EXCLUSION_STATUS = 'AC'
           END-EXEC

           IF SQLCODE = 0
               MOVE 'Y' TO WS-CRED-V-SAN-STATE
               MOVE 'FL' TO WS-CRED-V-SAN-STATUS
               MOVE 'C' TO ERR-SEVERITY
               MOVE 'CR0503' TO ERR-CODE
               MOVE 'STATE-MEDICAID-EXCL' TO ERR-FIELD-NAME
               MOVE PTR-NPI TO ERR-FIELD-VALUE
               MOVE 'PROVIDER ON STATE MEDICAID EXCLUSION LIST'
                   TO ERR-MESSAGE
               MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 3500-EXIT
           END-IF

      * 4. STATE LICENSE BOARD ACTION CHECK
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM LICENSE_ACTION_HISTORY
               WHERE LICENSE_NUMBER = :PTR-LICENSE-NUMBER
                 AND LICENSE_STATE = :PTR-LICENSE-STATE
                 AND ACTION_STATUS IN ('SU', 'RV')
                 AND (RESOLUTION_DATE IS NULL
                      OR RESOLUTION_DATE > CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0 AND HV-ROW-COUNT > 0
               MOVE 'FL' TO WS-CRED-V-SAN-STATUS
               MOVE 'C' TO ERR-SEVERITY
               MOVE 'CR0504' TO ERR-CODE
               MOVE 'LICENSE-SUSPENSION' TO ERR-FIELD-NAME
               MOVE PTR-LICENSE-NUMBER TO ERR-FIELD-VALUE
               MOVE 'ACTIVE LICENSE SUSPENSION OR REVOCATION'
                   TO ERR-MESSAGE
               MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 3500-EXIT
           END-IF

      * 5. MEDICARE OPT-OUT CHECK
           EXEC SQL
               SELECT OPTOUT_STATUS
               INTO :HV-MEDICARE-OPTOUT
               FROM MEDICARE_OPTOUT_LIST
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND OPTOUT_STATUS = 'Y'
                 AND (OPTOUT_END_DATE IS NULL
                      OR OPTOUT_END_DATE > CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               MOVE 'Y' TO WS-CRED-V-SAN-MCRE-OO
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'CR0505' TO ERR-CODE
               MOVE 'MEDICARE-OPTOUT' TO ERR-FIELD-NAME
               MOVE PTR-NPI TO ERR-FIELD-VALUE
               MOVE 'PROVIDER HAS OPTED OUT OF MEDICARE'
                   TO ERR-MESSAGE
               MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * 6. NSOPW CHECK FOR PEDIATRIC PROVIDERS
           IF WS-IS-PEDIATRIC
               EXEC SQL
                   SELECT CHECK_RESULT
                   INTO :HV-NSOPW-CHECK-RESULT
                   FROM NSOPW_CHECK_LOG
                   WHERE PROVIDER_NPI = :PTR-NPI
                   ORDER BY CHECK_DATE DESC
               END-EXEC

               IF SQLCODE = 0
                   IF HV-NSOPW-CHECK-RESULT NOT = 'CL'
                       MOVE 'Y' TO WS-CRED-V-SAN-NSOPW
                       MOVE 'FL' TO WS-CRED-V-SAN-STATUS
                       MOVE 'C' TO ERR-SEVERITY
                       MOVE 'CR0506' TO ERR-CODE
                       MOVE 'NSOPW-CHECK' TO ERR-FIELD-NAME
                       MOVE PTR-NPI TO ERR-FIELD-VALUE
                       MOVE 'PEDIATRIC PROVIDER NSOPW CHECK FAILED'
                           TO ERR-MESSAGE
                       MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                       GO TO 3500-EXIT
                   END-IF
               ELSE
                   IF SQLCODE = 100
                       MOVE 'W' TO ERR-SEVERITY
                       MOVE 'CR0507' TO ERR-CODE
                       MOVE 'NSOPW-MISSING' TO ERR-FIELD-NAME
                       MOVE PTR-NPI TO ERR-FIELD-VALUE
                       MOVE 'NO NSOPW CHECK ON FILE FOR PEDIATRIC '
                           'PROVIDER - QUERY REQUIRED'
                           TO ERR-MESSAGE
                       MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                   END-IF
               END-IF
           END-IF

      * 7. DEA SANCTION CHECK
           IF PTR-DEA-NUMBER NOT = SPACES
               EXEC SQL
                   SELECT COUNT(*)
                   INTO :HV-ROW-COUNT
                   FROM DEA_SANCTION_LOG
                   WHERE DEA_NUMBER = :PTR-DEA-NUMBER
                     AND SANCTION_STATUS = 'AC'
               END-EXEC

               IF SQLCODE = 0 AND HV-ROW-COUNT > 0
                   MOVE 'Y' TO WS-CRED-V-SAN-DEA
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'CR0508' TO ERR-CODE
                   MOVE 'DEA-SANCTION' TO ERR-FIELD-NAME
                   MOVE PTR-DEA-NUMBER TO ERR-FIELD-VALUE
                   MOVE 'DEA REGISTRATION HAS ACTIVE SANCTION'
                       TO ERR-MESSAGE
                   MOVE '3500-CHECK-SANCTIONS-EXCLUSIONS'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           END-IF

      * ALL CHECKS PASSED - ASSIGN SANCTION SCORE
           ADD WS-CRED-WEIGHT-SANCTION TO WS-CRED-SANCTION-SCORE

           IF WS-CRED-V-SAN-OIG = 'N' AND
              WS-CRED-V-SAN-SAM = 'N' AND
              WS-CRED-V-SAN-STATE = 'N' AND
              WS-CRED-V-SAN-DEA = 'N' AND
              WS-CRED-V-SAN-NSOPW = 'N'
               MOVE 'PS' TO WS-CRED-V-SAN-STATUS
           ELSE
               MOVE 'FL' TO WS-CRED-V-SAN-STATUS
               MOVE ZERO TO WS-CRED-SANCTION-SCORE
           END-IF

           MOVE WS-FORMATTED-DATE TO WS-CRED-V-SAN-DATE.

       3500-EXIT.
           EXIT.

      ****************************************************************
      * 3600-CALCULATE-CREDENTIAL-SCORE
      * WEIGHTED SCORING MODEL COMBINING ALL VERIFICATION RESULTS.
      * EDUCATION=20, LICENSE=25, BOARD=15, MALPRACTICE=15,
      * WORK HISTORY=10, REFERENCES=10, SANCTIONS=5.
      * TOTAL >= 70 = FULL APPROVAL, 55-69 = CONDITIONAL, <55 = DENY.
      ****************************************************************
       3600-CALCULATE-CREDENTIAL-SCORE.

      * CALCULATE REFERENCE SCORE (AUTOMATIC IF 3+ REFERENCES)
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM PROVIDER_REFERENCES
               WHERE PROVIDER_NPI = :PTR-NPI
                 AND REFERENCE_STATUS = 'VF'
           END-EXEC

           IF SQLCODE = 0
               MOVE HV-ROW-COUNT TO WS-CRED-V-REF-COUNT
               IF HV-ROW-COUNT >= 3
                   MOVE WS-CRED-WEIGHT-REFERENCE
                       TO WS-CRED-REFERENCE-SCORE
                   MOVE 'PS' TO WS-CRED-V-REF-STATUS
               ELSE
                   IF HV-ROW-COUNT >= 1
                       COMPUTE WS-CRED-REFERENCE-SCORE =
                           (HV-ROW-COUNT * WS-CRED-WEIGHT-REFERENCE)
                           / 3
                       MOVE 'PS' TO WS-CRED-V-REF-STATUS
                   ELSE
                       MOVE ZERO TO WS-CRED-REFERENCE-SCORE
                       MOVE 'FL' TO WS-CRED-V-REF-STATUS
                   END-IF
               END-IF
           ELSE
               MOVE ZERO TO WS-CRED-REFERENCE-SCORE
               MOVE 'PD' TO WS-CRED-V-REF-STATUS
           END-IF

           MOVE WS-FORMATTED-DATE TO WS-CRED-V-REF-DATE

      * CALCULATE TOTAL COMPOSITE SCORE
           COMPUTE WS-CRED-TOTAL-SCORE =
               WS-CRED-EDUCATION-SCORE +
               WS-CRED-LICENSE-SCORE +
               WS-CRED-BOARD-SCORE +
               WS-CRED-MALPRACT-SCORE +
               WS-CRED-WORKHIST-SCORE +
               WS-CRED-REFERENCE-SCORE +
               WS-CRED-SANCTION-SCORE

      * CAP AT MAXIMUM
           IF WS-CRED-TOTAL-SCORE > WS-CRED-MAX-SCORE
               MOVE WS-CRED-MAX-SCORE TO WS-CRED-TOTAL-SCORE
           END-IF

      * DETERMINE CREDENTIALING DECISION
           EVALUATE TRUE
               WHEN WS-CRED-TOTAL-SCORE >=
                   WS-CRED-PASS-THRESHOLD
      * FULL APPROVAL - ALL CRITICAL CHECKS MUST PASS
                   IF WS-CRED-V-SAN-PASS AND
                      (WS-CRED-V-LIC-PASS OR
                       WS-CRED-V-LIC-STATUS = 'NA')
                       MOVE 'FA' TO WS-CRED-FINAL-STATUS
                       SET WS-CRED-PASSED TO TRUE
                   ELSE
                       MOVE 'CA' TO WS-CRED-FINAL-STATUS
                   END-IF

               WHEN WS-CRED-TOTAL-SCORE >=
                   WS-CRED-CONDITIONAL-THRESH
      * CONDITIONAL APPROVAL
                   IF WS-CRED-V-SAN-PASS
                       MOVE 'CA' TO WS-CRED-FINAL-STATUS
                   ELSE
                       MOVE 'DN' TO WS-CRED-FINAL-STATUS
                   END-IF

               WHEN OTHER
      * DENIED
                   MOVE 'DN' TO WS-CRED-FINAL-STATUS
           END-EVALUATE

      * LOG THE SCORING DETAILS
           DISPLAY 'HCPRVMNT - CRED SCORE FOR ' HV-PROVIDER-ID
                   ': ED=' WS-CRED-EDUCATION-SCORE
                   ' LIC=' WS-CRED-LICENSE-SCORE
                   ' BRD=' WS-CRED-BOARD-SCORE
                   ' MAL=' WS-CRED-MALPRACT-SCORE
                   ' WRK=' WS-CRED-WORKHIST-SCORE
                   ' REF=' WS-CRED-REFERENCE-SCORE
                   ' SAN=' WS-CRED-SANCTION-SCORE
                   ' TOT=' WS-CRED-TOTAL-SCORE
                   ' DEC=' WS-CRED-FINAL-STATUS.

       3600-EXIT.
           EXIT.

      ****************************************************************
      * 3700-RECREDENTIALING-CHECK
      * MONITORS THE 3-YEAR RECREDENTIALING CYCLE. IDENTIFIES
      * PROVIDERS DUE FOR RECREDENTIALING, DETECTS GAPS IN THE
      * CREDENTIALING CYCLE, GENERATES EXPIRATION ALERTS, AND
      * TRACKS DELEGATED CREDENTIALING STATUS.
      ****************************************************************
       3700-RECREDENTIALING-CHECK.

      * CHECK IF PROVIDER HAS EXISTING CREDENTIALING
           EXEC SQL
               SELECT CRED_DATE, RECRED_DUE_DATE,
                      CRED_STATUS, CRED_SCORE
               INTO :HV-CRED-DATE, :HV-RECRED-DUE-DATE,
                    :HV-CRED-STATUS, :HV-CRED-SCORE
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               GO TO 3700-EXIT
           END-IF

      * CHECK IF RECREDENTIALING IS DUE
           IF HV-RECRED-DUE-DATE NOT = SPACES
               IF HV-RECRED-DUE-DATE <= WS-FORMATTED-DATE
                   ADD 1 TO WS-RECRED-DUE-CTR
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'CR0701' TO ERR-CODE
                   MOVE 'RECRED-DUE' TO ERR-FIELD-NAME
                   MOVE HV-RECRED-DUE-DATE TO ERR-FIELD-VALUE
                   MOVE 'RECREDENTIALING IS PAST DUE'
                       TO ERR-MESSAGE
                   MOVE '3700-RECREDENTIALING-CHECK'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           END-IF

      * CHECK FOR EXPIRED CREDENTIALS
           IF HV-CRED-STATUS = 'EX'
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'CR0702' TO ERR-CODE
               MOVE 'CRED-STATUS' TO ERR-FIELD-NAME
               MOVE 'EXPIRED' TO ERR-FIELD-VALUE
               MOVE 'CREDENTIALS HAVE EXPIRED - FULL RECRED REQUIRED'
                   TO ERR-MESSAGE
               MOVE '3700-RECREDENTIALING-CHECK'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * CHECK FOR DELEGATED CREDENTIALING STATUS
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM DELEGATED_CREDENTIALING
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND DELEGATION_STATUS = 'AC'
                 AND DELEGATION_EXP_DATE >= CURRENT_DATE
           END-EXEC

           IF SQLCODE = 0 AND HV-ROW-COUNT > 0
               SET WS-IS-DELEGATED TO TRUE
               ADD 1 TO WS-DELEGATED-CRED-CTR
           END-IF.

       3700-EXIT.
           EXIT.

      ****************************************************************
      * 3800-DELEGATED-CREDENTIALING
      * PROCESSES CREDENTIALING DECISIONS FROM DELEGATED ENTITIES
      * (IPAS, MEDICAL GROUPS). VERIFIES THE DELEGATION AGREEMENT
      * IS ACTIVE AND IN GOOD STANDING, VALIDATES THAT THE DELEGATED
      * ENTITY MEETS NCQA CREDENTIALING STANDARDS, AND MAINTAINS
      * A SEPARATE AUDIT TRAIL FOR DELEGATED DECISIONS.
      ****************************************************************
       3800-DELEGATED-CREDENTIALING.

      * VERIFY DELEGATION AGREEMENT STATUS
           EXEC SQL
               SELECT AGREEMENT_STATUS, AGREEMENT_DATE
               INTO :HV-DELEG-AGREEMENT-STATUS,
                    :HV-DELEG-AGREEMENT-DATE
               FROM DELEGATION_AGREEMENTS
               WHERE ENTITY_ID = :PTR-SOURCE-SYSTEM
                 AND AGREEMENT_STATUS = 'AC'
                 AND AGREEMENT_EXP_DATE >= CURRENT_DATE
           END-EXEC

           IF SQLCODE NOT = 0
               IF SQLCODE = 100
                   MOVE 'E' TO ERR-SEVERITY
                   MOVE 'DL0001' TO ERR-CODE
                   MOVE 'DELEGATION' TO ERR-FIELD-NAME
                   MOVE PTR-SOURCE-SYSTEM TO ERR-FIELD-VALUE
                   MOVE 'NO ACTIVE DELEGATION AGREEMENT FOUND'
                       TO ERR-MESSAGE
                   MOVE '3800-DELEGATED-CREDENTIALING'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
                   GO TO 3800-EXIT
               ELSE
                   PERFORM 8100-DATABASE-ERROR
                   GO TO 3800-EXIT
               END-IF
           END-IF

      * VERIFY DELEGATION AUDIT COMPLIANCE
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-ROW-COUNT
               FROM DELEGATION_AUDITS
               WHERE ENTITY_ID = :PTR-SOURCE-SYSTEM
                 AND AUDIT_RESULT = 'PS'
                 AND AUDIT_DATE >=
                     DATEADD(YEAR, -1, CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0 AND HV-ROW-COUNT = 0
               MOVE 'W' TO ERR-SEVERITY
               MOVE 'DL0002' TO ERR-CODE
               MOVE 'DELEGATION-AUDIT' TO ERR-FIELD-NAME
               MOVE PTR-SOURCE-SYSTEM TO ERR-FIELD-VALUE
               MOVE 'DELEGATED ENTITY HAS NO PASSING AUDIT IN 1 YEAR'
                   TO ERR-MESSAGE
               MOVE '3800-DELEGATED-CREDENTIALING'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * ACCEPT THE DELEGATED CREDENTIALING DECISION
           IF PTR-BOARD-CERT-STATUS = 'Y'
               MOVE 'FA' TO WS-CRED-FINAL-STATUS
           ELSE
               MOVE 'CA' TO WS-CRED-FINAL-STATUS
           END-IF

      * INSERT DELEGATED CREDENTIALING RECORD
           EXEC SQL
               INSERT INTO DELEGATED_CREDENTIALING
               (PROVIDER_ID, ENTITY_ID,
                DELEGATION_DATE, DELEGATION_DECISION,
                DELEGATION_STATUS, DELEGATION_EXP_DATE,
                CREATED_DATE, CREATED_BY)
               VALUES
               (:HV-PROVIDER-ID, :PTR-SOURCE-SYSTEM,
                CURRENT_TIMESTAMP, :WS-CRED-FINAL-STATUS,
                'AC',
                DATEADD(YEAR, 3, CURRENT_DATE),
                CURRENT_TIMESTAMP, :PTR-USER-ID)
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF

      * UPDATE PROVIDER MASTER
           EXEC SQL
               UPDATE PROVIDER_MASTER
               SET CRED_STATUS = :WS-CRED-FINAL-STATUS,
                   CRED_DATE = :WS-FORMATTED-DATE,
                   RECRED_DUE_DATE =
                       DATEADD(YEAR, 3, CURRENT_DATE),
                   LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                   LAST_UPDATE_USER = :PTR-USER-ID,
                   LAST_UPDATE_PGM = :WS-PROGRAM-NAME
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF

      * AUDIT TRAIL FOR DELEGATED CREDENTIALING
           MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
           MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
           MOVE PTR-NPI TO AUD-NPI
           MOVE 'DLG' TO AUD-ACTION-TYPE
           MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
           MOVE PTR-USER-ID TO AUD-USER-ID
           MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
           MOVE 'DELEGATED_CREDENTIALING' TO AUD-TABLE-NAME
           MOVE 'CRED_DECISION' TO AUD-FIELD-NAME
           MOVE SPACES TO AUD-BEFORE-VALUE
           MOVE WS-CRED-FINAL-STATUS TO AUD-AFTER-VALUE
           MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
           MOVE '00' TO AUD-RESULT-CODE
           STRING 'DELEGATED CRED FROM ' PTR-SOURCE-SYSTEM
               DELIMITED BY SIZE INTO AUD-RESULT-MSG
           END-STRING
           WRITE AUDIT-REC
           ADD 1 TO WS-AUDIT-CTR
           ADD 1 TO WS-DELEGATED-CRED-CTR.

       3800-EXIT.
           EXIT.

      ****************************************************************
      * 4000-NETWORK-ASSIGNMENT
      * MASTER NETWORK ASSIGNMENT PROCESSING. EVALUATES NETWORK
      * ADEQUACY, ASSIGNS OR REASSIGNS NETWORK TIER, MANAGES PCP
      * PANEL ASSIGNMENTS, AND HANDLES CONTRACT ASSOCIATIONS.
      ****************************************************************
       4000-NETWORK-ASSIGNMENT.

           SET WS-NETWORK-UNCHANGED TO TRUE

      * GET CURRENT PROVIDER NETWORK INFO
           IF HV-PROVIDER-ID = SPACES
               IF PTR-PROVIDER-ID NOT = SPACES
                   MOVE PTR-PROVIDER-ID TO HV-PROVIDER-ID
               ELSE
                   EXEC SQL
                       SELECT PROVIDER_ID
                       INTO :HV-PROVIDER-ID
                       FROM PROVIDER_MASTER
                       WHERE NPI = :PTR-NPI
                         AND STATUS = 'AC'
                   END-EXEC
                   IF SQLCODE NOT = 0
                       GO TO 4000-EXIT
                   END-IF
               END-IF
           END-IF

      * EVALUATE NETWORK ADEQUACY FOR THIS PROVIDER'S AREA
           PERFORM 4100-EVALUATE-NETWORK-ADEQUACY

      * ASSIGN OR REASSIGN NETWORK TIER
           PERFORM 4200-ASSIGN-NETWORK-TIER

      * MANAGE PCP PANEL IF APPLICABLE
           IF WS-IS-PCP
               PERFORM 4300-MANAGE-PCP-PANEL
           END-IF

           ADD 1 TO WS-NET-CTR.

       4000-EXIT.
           EXIT.

      ****************************************************************
      * 4100-EVALUATE-NETWORK-ADEQUACY
      * EVALUATES WHETHER THE PROVIDER'S SPECIALTY AND GEOGRAPHIC
      * AREA MEET ACCESS STANDARDS: DISTANCE/DRIVE TIME BY SPECIALTY,
      * PROVIDER-TO-MEMBER RATIOS, APPOINTMENT AVAILABILITY, AND
      * SPECIALTY COVERAGE GAPS. APPLIES DIFFERENT STANDARDS FOR
      * URBAN VS RURAL AREAS.
      ****************************************************************
       4100-EVALUATE-NETWORK-ADEQUACY.

           SET WS-NET-BELOW-STD TO TRUE

      * GET PROVIDER'S COUNTY AND SPECIALTY
           EXEC SQL
               SELECT PRACTICE_COUNTY, SPECIALTY_CODE,
                      TAXONOMY_CODE
               INTO :WS-NET-ADQ-COUNTY,
                    :WS-NET-ADQ-SPECIALTY,
                    :WS-WORK-STRING-1
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               GO TO 4100-EXIT
           END-IF

      * DETERMINE URBAN/RURAL CLASSIFICATION
           EXEC SQL
               SELECT URBAN_RURAL_IND
               INTO :WS-NET-ADQ-URBAN-RURAL
               FROM ZIP_COUNTY_MAPPING
               WHERE COUNTY_FIPS_CODE = :WS-NET-ADQ-COUNTY
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE 'U' TO WS-NET-ADQ-URBAN-RURAL
           END-IF

      * COUNT PROVIDERS IN THIS SPECIALTY/COUNTY
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-PROVIDER-COUNT
               FROM PROVIDER_MASTER
               WHERE PRACTICE_COUNTY = :WS-NET-ADQ-COUNTY
                 AND SPECIALTY_CODE = :WS-NET-ADQ-SPECIALTY
                 AND STATUS = 'AC'
                 AND CRED_STATUS IN ('FA', 'CA')
           END-EXEC

           IF SQLCODE = 0
               MOVE HV-PROVIDER-COUNT TO WS-NET-ADQ-PROV-COUNT
           END-IF

      * COUNT MEMBERS IN THIS COUNTY
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-MEMBER-COUNT
               FROM MEMBER_MASTER
               WHERE COUNTY_CODE = :WS-NET-ADQ-COUNTY
                 AND MEMBER_STATUS = 'AC'
           END-EXEC

           IF SQLCODE = 0
               MOVE HV-MEMBER-COUNT TO WS-NET-ADQ-MBR-COUNT
           END-IF

      * CALCULATE RATIO
           IF WS-NET-ADQ-PROV-COUNT > ZERO
               COMPUTE WS-NET-ADQ-RATIO =
                   WS-NET-ADQ-MBR-COUNT /
                   WS-NET-ADQ-PROV-COUNT
           ELSE
               MOVE 99999 TO WS-NET-ADQ-RATIO
           END-IF

      * EVALUATE AGAINST STANDARDS
           IF WS-NET-IS-URBAN
               IF WS-NET-ADQ-RATIO <=
                  WS-NET-ADQ-URB-RATIO
                   SET WS-NET-MEETS-STD TO TRUE
                   ADD 1 TO WS-NET-ADEQUATE-CTR
               ELSE
                   ADD 1 TO WS-NET-GAPS-CTR
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'NT0101' TO ERR-CODE
                   MOVE 'NET-ADEQUACY' TO ERR-FIELD-NAME
                   STRING WS-NET-ADQ-COUNTY ' '
                          WS-NET-ADQ-SPECIALTY
                       DELIMITED BY SIZE INTO ERR-FIELD-VALUE
                   END-STRING
                   MOVE 'URBAN NETWORK ADEQUACY BELOW STANDARD '
                       '- RATIO EXCEEDS LIMIT'
                       TO ERR-MESSAGE
                   MOVE '4100-EVALUATE-NETWORK-ADEQUACY'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           ELSE
               IF WS-NET-ADQ-RATIO <=
                  WS-NET-ADQ-RUR-RATIO
                   SET WS-NET-MEETS-STD TO TRUE
                   ADD 1 TO WS-NET-ADEQUATE-CTR
               ELSE
                   ADD 1 TO WS-NET-GAPS-CTR
                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'NT0102' TO ERR-CODE
                   MOVE 'NET-ADEQUACY' TO ERR-FIELD-NAME
                   STRING WS-NET-ADQ-COUNTY ' '
                          WS-NET-ADQ-SPECIALTY
                       DELIMITED BY SIZE INTO ERR-FIELD-VALUE
                   END-STRING
                   MOVE 'RURAL NETWORK ADEQUACY BELOW STANDARD'
                       TO ERR-MESSAGE
                   MOVE '4100-EVALUATE-NETWORK-ADEQUACY'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           END-IF.

       4100-EXIT.
           EXIT.

      ****************************************************************
      * 4200-ASSIGN-NETWORK-TIER
      * ASSIGNS TIERED NETWORK PLACEMENT BASED ON COMPOSITE SCORES
      * FOR QUALITY, COST EFFICIENCY, PATIENT SATISFACTION, AND
      * CLINICAL OUTCOMES. TIER 1 = PREFERRED, TIER 2 = STANDARD,
      * TIER 3 = BASIC. GENERATES TIER CHANGE NOTIFICATIONS.
      ****************************************************************
       4200-ASSIGN-NETWORK-TIER.

      * FETCH CURRENT QUALITY SCORES
           EXEC SQL
               SELECT COALESCE(QUALITY_SCORE, 0),
                      COALESCE(COST_EFFICIENCY_SCORE, 0),
                      COALESCE(PATIENT_SAT_SCORE, 0),
                      COALESCE(CLINICAL_OUTCOME_SCORE, 0),
                      TIER_LEVEL
               INTO :WS-NET-QUALITY-SCORE,
                    :WS-NET-COST-SCORE,
                    :WS-NET-SATISFACTION-SCORE,
                    :WS-NET-OUTCOMES-SCORE,
                    :WS-NET-PREVIOUS-TIER
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               GO TO 4200-EXIT
           END-IF

      * CALCULATE COMPOSITE SCORE (WEIGHTED AVERAGE)
      * QUALITY=30%, COST=30%, SATISFACTION=20%, OUTCOMES=20%
           COMPUTE WS-NET-COMPOSITE-SCORE =
               (WS-NET-QUALITY-SCORE * 0.30) +
               (WS-NET-COST-SCORE * 0.30) +
               (WS-NET-SATISFACTION-SCORE * 0.20) +
               (WS-NET-OUTCOMES-SCORE * 0.20)

      * DETERMINE TIER
           IF WS-NET-COMPOSITE-SCORE >=
              WS-NET-TIER-1-THRESHOLD
               MOVE '1' TO WS-NET-ASSIGNED-TIER
           ELSE
               IF WS-NET-COMPOSITE-SCORE >=
                  WS-NET-TIER-2-THRESHOLD
                   MOVE '2' TO WS-NET-ASSIGNED-TIER
               ELSE
                   MOVE '3' TO WS-NET-ASSIGNED-TIER
               END-IF
           END-IF

      * CHECK FOR TIER CHANGE
           IF WS-NET-ASSIGNED-TIER NOT = WS-NET-PREVIOUS-TIER
               MOVE 'Y' TO WS-NET-TIER-CHANGED-SW
               SET WS-NETWORK-CHANGED TO TRUE

      * TIER CHANGE NOTIFICATION
               EXEC SQL
                   INSERT INTO TIER_CHANGE_NOTIFICATIONS
                   (PROVIDER_ID, PREVIOUS_TIER, NEW_TIER,
                    COMPOSITE_SCORE, EFFECTIVE_DATE,
                    NOTIFICATION_STATUS, CREATED_DATE)
                   VALUES
                   (:HV-PROVIDER-ID, :WS-NET-PREVIOUS-TIER,
                    :WS-NET-ASSIGNED-TIER,
                    :WS-NET-COMPOSITE-SCORE,
                    DATEADD(DAY, 30, CURRENT_DATE),
                    'PD', CURRENT_TIMESTAMP)
               END-EXEC

               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF

      * UPDATE PROVIDER TIER
               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET TIER_LEVEL = :WS-NET-ASSIGNED-TIER,
                       QUALITY_SCORE = :WS-NET-COMPOSITE-SCORE,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC

               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF

      * AUDIT TRAIL
               MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
               MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
               MOVE PTR-NPI TO AUD-NPI
               MOVE 'NET' TO AUD-ACTION-TYPE
               MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
               MOVE PTR-USER-ID TO AUD-USER-ID
               MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
               MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
               MOVE 'TIER_LEVEL' TO AUD-FIELD-NAME
               MOVE WS-NET-PREVIOUS-TIER TO AUD-BEFORE-VALUE
               MOVE WS-NET-ASSIGNED-TIER TO AUD-AFTER-VALUE
               MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
               MOVE '00' TO AUD-RESULT-CODE
               MOVE 'TIER CHANGE PROCESSED' TO AUD-RESULT-MSG
               WRITE AUDIT-REC
               ADD 1 TO WS-AUDIT-CTR
           END-IF.

       4200-EXIT.
           EXIT.

      ****************************************************************
      * 4300-MANAGE-PCP-PANEL
      * MANAGES PRIMARY CARE PROVIDER PANEL ASSIGNMENTS INCLUDING
      * PANEL CAPACITY LIMITS, AUTO-ASSIGNMENT ALGORITHM BASED ON
      * GEOGRAPHIC PROXIMITY AND PANEL CAPACITY, PANEL LOCK/UNLOCK,
      * AND MEMBER NOTIFICATION OF PCP CHANGES.
      ****************************************************************
       4300-MANAGE-PCP-PANEL.

      * FETCH CURRENT PANEL INFO
           EXEC SQL
               SELECT PCP_PANEL_SIZE, PCP_PANEL_MAX,
                      ACCEPTING_NEW
               INTO :HV-PCP-PANEL-SIZE, :HV-PCP-PANEL-MAX,
                    :HV-ACCEPTING-NEW
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               GO TO 4300-EXIT
           END-IF

      * CALCULATE PANEL AVAILABILITY
           COMPUTE WS-PANEL-AVAILABLE =
               HV-PCP-PANEL-MAX - HV-PCP-PANEL-SIZE

      * CHECK IF PANEL SHOULD BE LOCKED/UNLOCKED
           IF WS-PANEL-AVAILABLE <= 0
      * PANEL IS FULL - LOCK IF NOT ALREADY
               IF HV-ACCEPTING-NEW = 'Y'
                   EXEC SQL
                       UPDATE PROVIDER_MASTER
                       SET ACCEPTING_NEW = 'N',
                           LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                           LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                       WHERE PROVIDER_ID = :HV-PROVIDER-ID
                   END-EXEC

                   MOVE 'W' TO ERR-SEVERITY
                   MOVE 'NT0301' TO ERR-CODE
                   MOVE 'PCP-PANEL' TO ERR-FIELD-NAME
                   MOVE HV-PCP-PANEL-SIZE TO WS-SQLCODE-DISPLAY
                   MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
                   MOVE 'PCP PANEL AT CAPACITY - AUTO-LOCKED'
                       TO ERR-MESSAGE
                   MOVE '4300-MANAGE-PCP-PANEL'
                       TO ERR-PARAGRAPH-NAME
                   PERFORM 8000-ERROR-HANDLER
               END-IF
           ELSE
      * PANEL HAS CAPACITY - UNLOCK IF LOCKED
               IF HV-ACCEPTING-NEW = 'N' AND
                  WS-PANEL-AVAILABLE > 50
                   EXEC SQL
                       UPDATE PROVIDER_MASTER
                       SET ACCEPTING_NEW = 'Y',
                           LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                           LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                       WHERE PROVIDER_ID = :HV-PROVIDER-ID
                   END-EXEC
               END-IF
           END-IF

      * UPDATE ACTUAL PANEL SIZE FROM MEMBER ASSIGNMENTS
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-MEMBER-COUNT
               FROM MEMBER_PCP_ASSIGNMENT
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND STATUS = 'AC'
                 AND (TERM_DATE IS NULL
                      OR TERM_DATE >= CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               IF HV-MEMBER-COUNT NOT = HV-PCP-PANEL-SIZE
                   EXEC SQL
                       UPDATE PROVIDER_MASTER
                       SET PCP_PANEL_SIZE = :HV-MEMBER-COUNT,
                           LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                           LAST_UPDATE_PGM = :WS-PROGRAM-NAME
                       WHERE PROVIDER_ID = :HV-PROVIDER-ID
                   END-EXEC
               END-IF
           END-IF.

       4300-EXIT.
           EXIT.

      ****************************************************************
      * 4400-CONTRACT-MANAGEMENT
      * PROCESSES CONTRACT CHANGES INCLUDING EFFECTIVE/TERM DATES,
      * CONTRACT AMENDMENTS, FEE SCHEDULE ATTACHMENTS, RATE
      * ESCALATION TRACKING, VALUE-BASED CONTRACT TERMS, SHARED
      * SAVINGS PARAMETERS, QUALITY BONUS CRITERIA, AND RISK
      * CORRIDOR CALCULATIONS.
      ****************************************************************
       4400-CONTRACT-MANAGEMENT.

           IF HV-PROVIDER-ID = SPACES
               IF PTR-PROVIDER-ID NOT = SPACES
                   MOVE PTR-PROVIDER-ID TO HV-PROVIDER-ID
               ELSE
                   EXEC SQL
                       SELECT PROVIDER_ID
                       INTO :HV-PROVIDER-ID
                       FROM PROVIDER_MASTER
                       WHERE NPI = :PTR-NPI
                         AND STATUS = 'AC'
                   END-EXEC
                   IF SQLCODE NOT = 0
                       GO TO 4400-EXIT
                   END-IF
               END-IF
           END-IF

      * VALIDATE CONTRACT ID
           IF PTR-CONTRACT-ID = SPACES
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'CT0001' TO ERR-CODE
               MOVE 'CONTRACT-ID' TO ERR-FIELD-NAME
               MOVE SPACES TO ERR-FIELD-VALUE
               MOVE 'CONTRACT ID IS REQUIRED FOR CONTRACT CHANGE'
                   TO ERR-MESSAGE
               MOVE '4400-CONTRACT-MANAGEMENT'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 4400-EXIT
           END-IF

      * VERIFY FEE SCHEDULE EXISTS
           IF PTR-FEE-SCHED-ID NOT = SPACES
               PERFORM VARYING WS-RATE-IDX FROM 1 BY 1
                   UNTIL WS-RATE-IDX > 50
                   IF WS-RATE-FEE-SCHED-ID(WS-RATE-IDX) =
                      PTR-FEE-SCHED-ID
                       SET WS-RATE-IDX TO 50
                   END-IF
               END-PERFORM
           END-IF

      * UPDATE CONTRACT INFORMATION
           EXEC SQL
               UPDATE PROVIDER_MASTER
               SET CONTRACT_ID = :PTR-CONTRACT-ID,
                   CONTRACT_EFF_DATE = :PTR-CONTRACT-EFF-DATE,
                   CONTRACT_TERM_DATE = :PTR-CONTRACT-TERM-DATE,
                   FEE_SCHEDULE_ID = :PTR-FEE-SCHED-ID,
                   NETWORK_ID = :PTR-NETWORK-ID,
                   WITHHOLD_PCT = :PTR-WITHHOLD-PCT,
                   QUALITY_BONUS_FLAG = :PTR-QUALITY-BONUS-FLAG,
                   VBC_FLAG = :PTR-VBC-FLAG,
                   SHARED_SAVINGS_PCT = :PTR-SHARED-SAVINGS-PCT,
                   LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                   LAST_UPDATE_USER = :PTR-USER-ID,
                   LAST_UPDATE_PGM = :WS-PROGRAM-NAME
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
               GO TO 4400-EXIT
           END-IF

      * INSERT CONTRACT HISTORY RECORD
           EXEC SQL
               INSERT INTO CONTRACT_HISTORY
               (PROVIDER_ID, CONTRACT_ID,
                EFFECTIVE_DATE, TERM_DATE,
                FEE_SCHEDULE_ID, NETWORK_ID,
                TIER_LEVEL, WITHHOLD_PCT,
                QUALITY_BONUS_FLAG, VBC_FLAG,
                SHARED_SAVINGS_PCT,
                ACTION_TYPE, ACTION_DATE, ACTION_BY)
               VALUES
               (:HV-PROVIDER-ID, :PTR-CONTRACT-ID,
                :PTR-CONTRACT-EFF-DATE, :PTR-CONTRACT-TERM-DATE,
                :PTR-FEE-SCHED-ID, :PTR-NETWORK-ID,
                :PTR-TIER-LEVEL, :PTR-WITHHOLD-PCT,
                :PTR-QUALITY-BONUS-FLAG, :PTR-VBC-FLAG,
                :PTR-SHARED-SAVINGS-PCT,
                'UPD', CURRENT_TIMESTAMP, :PTR-USER-ID)
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF

      * AUDIT TRAIL
           MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
           MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
           MOVE PTR-NPI TO AUD-NPI
           MOVE 'CTR' TO AUD-ACTION-TYPE
           MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
           MOVE PTR-USER-ID TO AUD-USER-ID
           MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
           MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
           MOVE 'CONTRACT_ID' TO AUD-FIELD-NAME
           MOVE WS-BEF-CONTRACT-ID TO AUD-BEFORE-VALUE
           MOVE PTR-CONTRACT-ID TO AUD-AFTER-VALUE
           MOVE PTR-CONTRACT-EFF-DATE TO AUD-EFF-DATE
           MOVE '00' TO AUD-RESULT-CODE
           MOVE 'CONTRACT CHANGE PROCESSED' TO AUD-RESULT-MSG
           WRITE AUDIT-REC
           ADD 1 TO WS-AUDIT-CTR

           ADD 1 TO WS-CONTRACT-CHANGE-CTR.

       4400-EXIT.
           EXIT.

      ****************************************************************
      * 5000-SETUP-PAYMENT-METHOD
      * MASTER PAYMENT SETUP PROCESSING. ROUTES TO BANKING
      * VALIDATION, TAX REPORTING SETUP, AND WITHHOLD CONFIGURATION.
      ****************************************************************
       5000-SETUP-PAYMENT-METHOD.

           IF HV-PROVIDER-ID = SPACES
               IF PTR-PROVIDER-ID NOT = SPACES
                   MOVE PTR-PROVIDER-ID TO HV-PROVIDER-ID
               ELSE
                   EXEC SQL
                       SELECT PROVIDER_ID
                       INTO :HV-PROVIDER-ID
                       FROM PROVIDER_MASTER
                       WHERE NPI = :PTR-NPI
                         AND STATUS = 'AC'
                   END-EXEC
                   IF SQLCODE NOT = 0
                       GO TO 5000-EXIT
                   END-IF
               END-IF
           END-IF

      * PROCESS BANKING INFORMATION IF EFT
           IF PTR-PAY-EFT
               PERFORM 5100-VALIDATE-BANKING-INFO
           END-IF

      * PROCESS TAX REPORTING SETUP
           PERFORM 5200-TAX-REPORTING-SETUP

      * PROCESS WITHHOLD CONFIGURATION
           IF PTR-WITHHOLD-PCT > ZERO
               PERFORM 5300-WITHHOLD-CONFIGURATION
           END-IF

           ADD 1 TO WS-PAY-CTR.

       5000-EXIT.
           EXIT.

      ****************************************************************
      * 5100-VALIDATE-BANKING-INFO
      * VALIDATES BANK ROUTING NUMBER VIA ABA CHECK DIGIT ALGORITHM,
      * VALIDATES ACCOUNT NUMBER FORMAT, GENERATES PRENOTE FOR NEW
      * EFT ACCOUNTS, ACTIVATES EFT AFTER PRENOTE VERIFICATION,
      * AND IMPLEMENTS BANK CHANGE FRAUD DETECTION VIA VELOCITY CHECK.
      ****************************************************************
       5100-VALIDATE-BANKING-INFO.

           SET WS-BANKING-INVALID TO TRUE

      * VALIDATE ABA ROUTING NUMBER CHECK DIGIT
      * ALGORITHM: 3(D1) + 7(D2) + 1(D3) + 3(D4) + 7(D5) +
      *            1(D6) + 3(D7) + 7(D8) + 1(D9) = 0 MOD 10
           MOVE PTR-BANK-ROUTING TO WS-ABA-ROUTING

           IF WS-ABA-ROUTING IS NOT NUMERIC
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'PY0101' TO ERR-CODE
               MOVE 'BANK-ROUTING' TO ERR-FIELD-NAME
               MOVE WS-ABA-ROUTING TO ERR-FIELD-VALUE
               MOVE 'ROUTING NUMBER IS NOT NUMERIC'
                   TO ERR-MESSAGE
               MOVE '5100-VALIDATE-BANKING-INFO'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 5100-EXIT
           END-IF

      * EXTRACT INDIVIDUAL DIGITS
           PERFORM VARYING WS-WORK-INDEX FROM 1 BY 1
               UNTIL WS-WORK-INDEX > 9
               MOVE WS-ABA-ROUTING(WS-WORK-INDEX:1)
                   TO WS-ABA-DIG(WS-WORK-INDEX)
           END-PERFORM

      * VALIDATE FEDERAL RESERVE DISTRICT (FIRST 2 DIGITS)
           COMPUTE WS-ABA-FED-DISTRICT =
               (WS-ABA-DIG(1) * 10) + WS-ABA-DIG(2)

           IF NOT (WS-ABA-VALID-DIST OR
                   WS-ABA-THRIFT OR
                   WS-ABA-ELECTRONIC)
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'PY0102' TO ERR-CODE
               MOVE 'ROUTING-PREFIX' TO ERR-FIELD-NAME
               MOVE WS-ABA-FED-DISTRICT TO WS-SQLCODE-DISPLAY
               MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
               MOVE 'INVALID FEDERAL RESERVE ROUTING PREFIX'
                   TO ERR-MESSAGE
               MOVE '5100-VALIDATE-BANKING-INFO'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 5100-EXIT
           END-IF

      * APPLY ABA CHECK DIGIT ALGORITHM
           COMPUTE WS-ABA-WEIGHTED-SUM =
               (3 * WS-ABA-DIG(1)) +
               (7 * WS-ABA-DIG(2)) +
               (1 * WS-ABA-DIG(3)) +
               (3 * WS-ABA-DIG(4)) +
               (7 * WS-ABA-DIG(5)) +
               (1 * WS-ABA-DIG(6)) +
               (3 * WS-ABA-DIG(7)) +
               (7 * WS-ABA-DIG(8)) +
               (1 * WS-ABA-DIG(9))

           DIVIDE WS-ABA-WEIGHTED-SUM BY 10
               GIVING WS-WORK-QUOTIENT
               REMAINDER WS-ABA-MOD-RESULT
           END-DIVIDE

           IF WS-ABA-MOD-RESULT NOT = ZERO
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'PY0103' TO ERR-CODE
               MOVE 'ROUTING-CHECK' TO ERR-FIELD-NAME
               MOVE WS-ABA-ROUTING TO ERR-FIELD-VALUE
               MOVE 'ABA ROUTING NUMBER FAILS CHECK DIGIT VALIDATION'
                   TO ERR-MESSAGE
               MOVE '5100-VALIDATE-BANKING-INFO'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 5100-EXIT
           END-IF

      * VALIDATE ACCOUNT NUMBER FORMAT (NOT EMPTY, REASONABLE LENGTH)
           IF PTR-BANK-ACCOUNT = SPACES
               MOVE 'E' TO ERR-SEVERITY
               MOVE 'PY0110' TO ERR-CODE
               MOVE 'BANK-ACCOUNT' TO ERR-FIELD-NAME
               MOVE SPACES TO ERR-FIELD-VALUE
               MOVE 'BANK ACCOUNT NUMBER IS REQUIRED FOR EFT'
                   TO ERR-MESSAGE
               MOVE '5100-VALIDATE-BANKING-INFO'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
               GO TO 5100-EXIT
           END-IF

      * FRAUD DETECTION - VELOCITY CHECK ON BANK CHANGES
      * CHECK IF BANKING INFO CHANGED RECENTLY (< 30 DAYS)
           EXEC SQL
               SELECT COUNT(*), MAX(CHANGE_DATE)
               INTO :HV-BANK-CHANGE-COUNT,
                    :HV-BANK-CHANGE-DATE
               FROM PROVIDER_BANK_CHANGE_LOG
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND CHANGE_DATE >=
                     DATEADD(DAY, -30, CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0 AND HV-BANK-CHANGE-COUNT >= 2
               MOVE 'C' TO ERR-SEVERITY
               MOVE 'PY0120' TO ERR-CODE
               MOVE 'BANK-VELOCITY' TO ERR-FIELD-NAME
               MOVE HV-BANK-CHANGE-COUNT TO WS-SQLCODE-DISPLAY
               MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
               MOVE 'MULTIPLE BANK CHANGES IN 30 DAYS - '
                   'POTENTIAL FRAUD - MANUAL REVIEW REQUIRED'
                   TO ERR-MESSAGE
               MOVE '5100-VALIDATE-BANKING-INFO'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * LOG BANK CHANGE
           EXEC SQL
               SELECT BANK_ROUTING, BANK_ACCOUNT
               INTO :WS-PREV-BANK-ROUTING, :WS-PREV-BANK-ACCOUNT
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE = 0
               EXEC SQL
                   INSERT INTO PROVIDER_BANK_CHANGE_LOG
                   (PROVIDER_ID, PREV_ROUTING, PREV_ACCOUNT,
                    NEW_ROUTING, NEW_ACCOUNT,
                    CHANGE_DATE, CHANGED_BY, SOURCE_SYSTEM)
                   VALUES
                   (:HV-PROVIDER-ID,
                    :WS-PREV-BANK-ROUTING,
                    :WS-PREV-BANK-ACCOUNT,
                    :PTR-BANK-ROUTING, :PTR-BANK-ACCOUNT,
                    CURRENT_TIMESTAMP, :PTR-USER-ID,
                    :PTR-SOURCE-SYSTEM)
               END-EXEC
           END-IF

      * BANKING IS VALID - GENERATE PRENOTE
           SET WS-BANKING-VALID TO TRUE

      * UPDATE PROVIDER MASTER WITH NEW BANKING
           EXEC SQL
               UPDATE PROVIDER_MASTER
               SET PAY_METHOD = 'E',
                   EFT_STATUS = 'PN',
                   BANK_ROUTING = :PTR-BANK-ROUTING,
                   BANK_ACCOUNT = :PTR-BANK-ACCOUNT,
                   BANK_ACCT_TYPE = :PTR-BANK-ACCT-TYPE,
                   LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                   LAST_UPDATE_USER = :PTR-USER-ID,
                   LAST_UPDATE_PGM = :WS-PROGRAM-NAME
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
               GO TO 5100-EXIT
           END-IF

      * WRITE PRENOTE RECORD TO PAYMENT FILE
           MOVE HV-PROVIDER-ID TO PPR-PROVIDER-ID
           MOVE PTR-NPI TO PPR-NPI
           MOVE PTR-TAX-ID TO PPR-TAX-ID
           MOVE 'E' TO PPR-PAY-METHOD
           MOVE PTR-BANK-ROUTING TO PPR-BANK-ROUTING
           MOVE PTR-BANK-ACCOUNT TO PPR-BANK-ACCOUNT
           MOVE PTR-BANK-ACCT-TYPE TO PPR-BANK-ACCT-TYPE
           MOVE 'P' TO PPR-PRENOTE-STATUS
           MOVE WS-FORMATTED-DATE TO PPR-PRENOTE-DATE
           MOVE SPACES TO PPR-EFT-ACTIVE-DATE
           MOVE PTR-WITHHOLD-PCT TO PPR-WITHHOLD-PCT
           MOVE WS-FORMATTED-DATE TO PPR-EFF-DATE
           MOVE 'P' TO PPR-ACTION-CODE

           WRITE PRV-PAY-REC
           ADD 1 TO WS-PRENOTE-CTR
           ADD 1 TO WS-EFT-SETUP-CTR

      * AUDIT TRAIL
           MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
           MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
           MOVE PTR-NPI TO AUD-NPI
           MOVE 'PAY' TO AUD-ACTION-TYPE
           MOVE PTR-TRANS-SEQ-NO TO AUD-TRANS-SEQ
           MOVE PTR-USER-ID TO AUD-USER-ID
           MOVE PTR-SOURCE-SYSTEM TO AUD-SOURCE-SYSTEM
           MOVE 'PROVIDER_MASTER' TO AUD-TABLE-NAME
           MOVE 'EFT_SETUP' TO AUD-FIELD-NAME
           MOVE 'CHECK' TO AUD-BEFORE-VALUE
           MOVE 'EFT-PRENOTE' TO AUD-AFTER-VALUE
           MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
           MOVE '00' TO AUD-RESULT-CODE
           MOVE 'EFT SETUP WITH PRENOTE INITIATED' TO AUD-RESULT-MSG
           WRITE AUDIT-REC
           ADD 1 TO WS-AUDIT-CTR.

       5100-EXIT.
           EXIT.

      ****************************************************************
      * 5200-TAX-REPORTING-SETUP
      * HANDLES W-9 INFORMATION CAPTURE, TIN/EIN VALIDATION,
      * 1099 REPORTING THRESHOLD TRACKING ($600), BACKUP WITHHOLDING
      * DETERMINATION (24% IF NO W-9), 1099-MISC VS 1099-NEC
      * DETERMINATION, CORRECTION PROCESSING, AND B-NOTICE HANDLING.
      ****************************************************************
       5200-TAX-REPORTING-SETUP.

      * CHECK W-9 STATUS
           IF PTR-W9-RECEIVED = 'Y'
               MOVE 'Y' TO PPR-W9-STATUS
               MOVE PTR-W9-DATE TO PPR-W9-DATE

      * DETERMINE 1099 TYPE
      * AS OF 2020: NEC FOR NONEMPLOYEE COMPENSATION
      * MISC FOR RENTS, ROYALTIES, OTHER INCOME
               IF PTR-ENTITY-TYPE = '1'
                   MOVE 'NEC' TO WS-1099-TYPE-CODE
               ELSE
                   MOVE 'NEC' TO WS-1099-TYPE-CODE
               END-IF

               MOVE 'N' TO PPR-BACKUP-WITHHOLD

      * CHECK FOR B-NOTICE (IRS NOTIFICATION OF TIN MISMATCH)
               EXEC SQL
                   SELECT COUNT(*), MAX(NOTICE_DATE)
                   INTO :WS-1099-B-NOTICE-COUNT,
                        :WS-1099-B-NOTICE-DATE
                   FROM TAX_B_NOTICES
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
                     AND NOTICE_STATUS = 'AC'
               END-EXEC

               IF SQLCODE = 0 AND WS-1099-B-NOTICE-COUNT > 0
                   MOVE 'Y' TO WS-1099-B-NOTICE-SW
      * SECOND B-NOTICE TRIGGERS BACKUP WITHHOLDING
                   IF WS-1099-B-NOTICE-COUNT >= 2
                       MOVE 'Y' TO PPR-BACKUP-WITHHOLD
                       MOVE 'W' TO ERR-SEVERITY
                       MOVE 'PY0201' TO ERR-CODE
                       MOVE 'B-NOTICE' TO ERR-FIELD-NAME
                       MOVE WS-1099-B-NOTICE-COUNT
                           TO WS-SQLCODE-DISPLAY
                       MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE
                       MOVE 'SECOND B-NOTICE RECEIVED - BACKUP '
                           'WITHHOLDING AT 24% ACTIVATED'
                           TO ERR-MESSAGE
                       MOVE '5200-TAX-REPORTING-SETUP'
                           TO ERR-PARAGRAPH-NAME
                       PERFORM 8000-ERROR-HANDLER
                   END-IF
               END-IF
           ELSE
      * NO W-9 ON FILE - BACKUP WITHHOLDING REQUIRED
               MOVE 'N' TO PPR-W9-STATUS
               MOVE 'Y' TO PPR-BACKUP-WITHHOLD
               MOVE 'Y' TO PTR-BACKUP-WITHHOLD

               MOVE 'W' TO ERR-SEVERITY
               MOVE 'PY0200' TO ERR-CODE
               MOVE 'W9-STATUS' TO ERR-FIELD-NAME
               MOVE 'MISSING' TO ERR-FIELD-VALUE
               MOVE 'W-9 NOT RECEIVED - 24% BACKUP WITHHOLDING '
                   'WILL BE APPLIED PER IRS REGULATIONS'
                   TO ERR-MESSAGE
               MOVE '5200-TAX-REPORTING-SETUP'
                   TO ERR-PARAGRAPH-NAME
               PERFORM 8000-ERROR-HANDLER
           END-IF

      * GET YTD PAYMENT TOTALS FOR 1099 THRESHOLD CHECK
           EXEC SQL
               SELECT COALESCE(SUM(PAID_AMOUNT), 0)
               INTO :HV-YTD-TOTAL-PAID
               FROM CLAIMS_PAYMENT
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND PAYMENT_YEAR = YEAR(CURRENT_DATE)
           END-EXEC

           IF SQLCODE = 0
               MOVE HV-YTD-TOTAL-PAID TO WS-1099-YTD-PAYMENTS
               MOVE HV-YTD-TOTAL-PAID TO PPR-YTD-PAYMENTS
           END-IF

      * UPDATE TAX INFO IN DATABASE
           EXEC SQL
               UPDATE PROVIDER_MASTER
               SET W9_RECEIVED = :PTR-W9-RECEIVED,
                   W9_DATE = :PTR-W9-DATE,
                   BACKUP_WITHHOLD = :PTR-BACKUP-WITHHOLD,
                   LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                   LAST_UPDATE_USER = :PTR-USER-ID
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF.

       5200-EXIT.
           EXIT.

      ****************************************************************
      * 5300-WITHHOLD-CONFIGURATION
      * CONFIGURES WITHHOLD PERCENTAGE BY CONTRACT, TRACKS WITHHOLD
      * FUND BALANCE, SCHEDULES RECONCILIATION, AND CALCULATES
      * YEAR-END DISTRIBUTION INCLUDING QUALITY BONUS INTEGRATION.
      ****************************************************************
       5300-WITHHOLD-CONFIGURATION.

      * FETCH CURRENT WITHHOLD CONFIGURATION
           EXEC SQL
               SELECT WITHHOLD_PCT,
                      COALESCE(WITHHOLD_BALANCE, 0),
                      QUALITY_BONUS_FLAG
               INTO :HV-WITHHOLD-PCT,
                    :PPR-WITHHOLD-BALANCE,
                    :HV-QUALITY-BONUS-FLAG
               FROM PROVIDER_MASTER
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
           END-EXEC

           IF SQLCODE NOT = 0
               GO TO 5300-EXIT
           END-IF

      * UPDATE WITHHOLD PERCENTAGE IF CHANGED
           IF PTR-WITHHOLD-PCT NOT = HV-WITHHOLD-PCT
               EXEC SQL
                   UPDATE PROVIDER_MASTER
                   SET WITHHOLD_PCT = :PTR-WITHHOLD-PCT,
                       LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                       LAST_UPDATE_USER = :PTR-USER-ID
                   WHERE PROVIDER_ID = :HV-PROVIDER-ID
               END-EXEC

               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF

      * LOG WITHHOLD CHANGE
               EXEC SQL
                   INSERT INTO WITHHOLD_HISTORY
                   (PROVIDER_ID, PREV_WITHHOLD_PCT,
                    NEW_WITHHOLD_PCT, EFFECTIVE_DATE,
                    CHANGED_BY, CHANGE_DATE)
                   VALUES
                   (:HV-PROVIDER-ID, :HV-WITHHOLD-PCT,
                    :PTR-WITHHOLD-PCT, :WS-FORMATTED-DATE,
                    :PTR-USER-ID, CURRENT_TIMESTAMP)
               END-EXEC

               IF SQLCODE NOT = 0
                   PERFORM 8100-DATABASE-ERROR
               END-IF
           END-IF

      * CHECK FOR YEAR-END WITHHOLD DISTRIBUTION ELIGIBILITY
      * QUALITY BONUS INTEGRATION: IF QUALITY TARGETS MET,
      * RETURN 100% OF WITHHOLD + BONUS. OTHERWISE PRORATE.
           IF WS-CURRENT-MONTH = 12 AND WS-CURRENT-DAY >= 15
               IF HV-QUALITY-BONUS-FLAG = 'Y'
                   EXEC SQL
                       SELECT QUALITY_SCORE
                       INTO :HV-QUALITY-SCORE
                       FROM PROVIDER_MASTER
                       WHERE PROVIDER_ID = :HV-PROVIDER-ID
                   END-EXEC

                   IF SQLCODE = 0
                       IF HV-QUALITY-SCORE >= 80.00
      * FULL WITHHOLD RETURN + QUALITY BONUS
                           EXEC SQL
                               INSERT INTO WITHHOLD_DISTRIBUTION
                               (PROVIDER_ID, DISTRIBUTION_YEAR,
                                WITHHOLD_BALANCE,
                                DISTRIBUTION_PCT,
                                QUALITY_BONUS_AMT,
                                DISTRIBUTION_DATE, STATUS)
                               VALUES
                               (:HV-PROVIDER-ID,
                                YEAR(CURRENT_DATE),
                                :PPR-WITHHOLD-BALANCE,
                                100,
                                :PPR-WITHHOLD-BALANCE * 0.10,
                                CURRENT_TIMESTAMP, 'PD')
                           END-EXEC
                       ELSE
      * PRORATED WITHHOLD RETURN BASED ON QUALITY
                           EXEC SQL
                               INSERT INTO WITHHOLD_DISTRIBUTION
                               (PROVIDER_ID, DISTRIBUTION_YEAR,
                                WITHHOLD_BALANCE,
                                DISTRIBUTION_PCT,
                                QUALITY_BONUS_AMT,
                                DISTRIBUTION_DATE, STATUS)
                               VALUES
                               (:HV-PROVIDER-ID,
                                YEAR(CURRENT_DATE),
                                :PPR-WITHHOLD-BALANCE,
                                :HV-QUALITY-SCORE,
                                0,
                                CURRENT_TIMESTAMP, 'PD')
                           END-EXEC
                       END-IF
                   END-IF
               END-IF
           END-IF.

       5300-EXIT.
           EXIT.

      ****************************************************************
      * 6000-PROCESS-OIG-FILE
      * READS THE OIG LEIE (LIST OF EXCLUDED INDIVIDUALS/ENTITIES)
      * MONTHLY FILE AND MATCHES AGAINST THE ACTIVE PROVIDER MASTER.
      * THE LEIE FILE IS PUBLISHED MONTHLY BY THE HHS OIG AND
      * CONTAINS ALL EXCLUDED INDIVIDUALS AND ENTITIES.
      ****************************************************************
       6000-PROCESS-OIG-FILE.

           DISPLAY 'HCPRVMNT - BEGINNING OIG EXCLUSION PROCESSING'

           SET WS-NOT-END-OIG TO TRUE

           PERFORM UNTIL WS-END-OF-OIG
               READ OIG-EXCLUSION-FILE
                   AT END
                       SET WS-END-OF-OIG TO TRUE
                   NOT AT END
                       ADD 1 TO WS-OIG-RECORDS-READ
                       PERFORM 6100-MATCH-OIG-PROVIDERS
               END-READ
           END-PERFORM

           DISPLAY 'HCPRVMNT - OIG RECORDS READ:     '
                   WS-OIG-RECORDS-READ
           DISPLAY 'HCPRVMNT - OIG MATCHES FOUND:    '
                   WS-OIG-MATCHES-FOUND
           DISPLAY 'HCPRVMNT - OIG EXACT MATCHES:    '
                   WS-OIG-EXACT-MATCHES
           DISPLAY 'HCPRVMNT - OIG FUZZY MATCHES:    '
                   WS-OIG-FUZZY-MATCHES.

      ****************************************************************
      * 6100-MATCH-OIG-PROVIDERS
      * PERFORMS EXACT AND FUZZY MATCHING OF OIG EXCLUSION RECORDS
      * AGAINST THE PROVIDER MASTER. EXACT MATCH ON NPI OR
      * NAME+SSN/EIN. FUZZY MATCH ON NAME+DOB+STATE WITH
      * CONFIDENCE SCORING. FALSE POSITIVE HANDLING.
      ****************************************************************
       6100-MATCH-OIG-PROVIDERS.

           INITIALIZE WS-OIG-MATCH-WORK
           SET WS-OIG-NO-MATCH TO TRUE
           MOVE ZERO TO WS-OIG-MATCH-SCORE

      * ATTEMPT 1: EXACT MATCH ON NPI
           IF OIG-NPI NOT = SPACES
               EXEC SQL
                   SELECT PROVIDER_ID, NPI, LAST_NAME,
                          FIRST_NAME, TAX_ID
                   INTO :HV-PROVIDER-ID, :HV-NPI,
                        :HV-LAST-NAME, :HV-FIRST-NAME,
                        :HV-TAX-ID
                   FROM PROVIDER_MASTER
                   WHERE NPI = :OIG-NPI
                     AND STATUS IN ('AC', 'SU')
               END-EXEC

               IF SQLCODE = 0
                   SET WS-OIG-EXACT-MATCH TO TRUE
                   MOVE 100 TO WS-OIG-MATCH-SCORE
                   MOVE 'Y' TO WS-OIG-NPI-MATCH
                   ADD 1 TO WS-OIG-MATCHES-FOUND
                   ADD 1 TO WS-OIG-EXACT-MATCHES
                   PERFORM 6400-EXCLUSION-ACTION-PROCESSING
                   GO TO 6100-EXIT
               END-IF
           END-IF

      * ATTEMPT 2: EXACT MATCH ON NAME + SSN/EIN
           IF OIG-SSN NOT = SPACES
               EXEC SQL
                   SELECT PROVIDER_ID, NPI, LAST_NAME,
                          FIRST_NAME, TAX_ID
                   INTO :HV-PROVIDER-ID, :HV-NPI,
                        :HV-LAST-NAME, :HV-FIRST-NAME,
                        :HV-TAX-ID
                   FROM PROVIDER_MASTER
                   WHERE TAX_ID = :OIG-SSN
                     AND UPPER(LAST_NAME) = UPPER(:OIG-LAST-NAME)
                     AND STATUS IN ('AC', 'SU')
               END-EXEC

               IF SQLCODE = 0
                   SET WS-OIG-EXACT-MATCH TO TRUE
                   MOVE 100 TO WS-OIG-MATCH-SCORE
                   MOVE 'Y' TO WS-OIG-SSN-MATCH
                   ADD 1 TO WS-OIG-MATCHES-FOUND
                   ADD 1 TO WS-OIG-EXACT-MATCHES
                   PERFORM 6400-EXCLUSION-ACTION-PROCESSING
                   GO TO 6100-EXIT
               END-IF
           END-IF

      * ATTEMPT 3: FUZZY MATCH ON NAME + DOB + STATE
           IF OIG-LAST-NAME NOT = SPACES AND
              OIG-DOB NOT = SPACES
               EXEC SQL
                   DECLARE OIG_FUZZY_CURSOR CURSOR FOR
                   SELECT PROVIDER_ID, NPI, LAST_NAME,
                          FIRST_NAME, DOB, PRACTICE_STATE
                   FROM PROVIDER_MASTER
                   WHERE UPPER(LAST_NAME) = UPPER(:OIG-LAST-NAME)
                     AND STATUS IN ('AC', 'SU')
               END-EXEC

               EXEC SQL
                   OPEN OIG_FUZZY_CURSOR
               END-EXEC

               IF SQLCODE = 0
                   PERFORM UNTIL SQLCODE NOT = 0
                       EXEC SQL
                           FETCH OIG_FUZZY_CURSOR
                           INTO :HV-PROVIDER-ID, :HV-NPI,
                                :HV-LAST-NAME, :HV-FIRST-NAME,
                                :HV-DOB, :HV-PRACTICE-STATE
                       END-EXEC

                       IF SQLCODE = 0
                           MOVE ZERO TO WS-OIG-MATCH-SCORE
                           ADD 30 TO WS-OIG-MATCH-SCORE

      * CHECK FIRST NAME MATCH
                           MOVE OIG-FIRST-NAME TO WS-FUZZY-STR-1
                           MOVE HV-FIRST-NAME TO WS-FUZZY-STR-2
                           PERFORM 6150-FUZZY-NAME-MATCH
                           ADD WS-FUZZY-RESULT-PCT
                               TO WS-OIG-MATCH-SCORE

      * CHECK DOB MATCH
                           IF OIG-DOB = HV-DOB
                               ADD 30 TO WS-OIG-MATCH-SCORE
                               MOVE 'Y' TO WS-OIG-DOB-MATCH
                           END-IF

      * CHECK STATE MATCH
                           IF OIG-STATE = HV-PRACTICE-STATE
                               ADD 10 TO WS-OIG-MATCH-SCORE
                               MOVE 'Y' TO WS-OIG-STATE-MATCH
                           END-IF

      * EVALUATE MATCH SCORE
                           IF WS-OIG-MATCH-SCORE >=
                              WS-OIG-MATCH-THRESHOLD
                               SET WS-OIG-FUZZY-MATCH TO TRUE
                               ADD 1 TO WS-OIG-MATCHES-FOUND
                               ADD 1 TO WS-OIG-FUZZY-MATCHES
                               PERFORM
                                   6400-EXCLUSION-ACTION-PROCESSING
                           END-IF
                       END-IF
                   END-PERFORM

                   EXEC SQL
                       CLOSE OIG_FUZZY_CURSOR
                   END-EXEC
               END-IF
           END-IF.

       6100-EXIT.
           EXIT.

      ****************************************************************
      * 6150-FUZZY-NAME-MATCH
      * PERFORMS FUZZY NAME MATCHING USING A SIMPLE CHARACTER-BY-
      * CHARACTER COMPARISON AND SOUNDEX EQUIVALENCE CHECK.
      * RETURNS A PERCENTAGE MATCH SCORE.
      ****************************************************************
       6150-FUZZY-NAME-MATCH.

           MOVE ZERO TO WS-FUZZY-RESULT-PCT
           MOVE ZERO TO WS-FUZZY-MATCH-CHARS

      * CALCULATE LENGTHS (IGNORING TRAILING SPACES)
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-FUZZY-STR-1))
               TO WS-FUZZY-LEN-1
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-FUZZY-STR-2))
               TO WS-FUZZY-LEN-2

           IF WS-FUZZY-LEN-1 = 0 OR WS-FUZZY-LEN-2 = 0
               GO TO 6150-EXIT
           END-IF

      * SIMPLE CHARACTER COMPARISON
           MOVE FUNCTION MIN(WS-FUZZY-LEN-1, WS-FUZZY-LEN-2)
               TO WS-FUZZY-TOTAL-CHARS

           PERFORM VARYING WS-FUZZY-IDX-1 FROM 1 BY 1
               UNTIL WS-FUZZY-IDX-1 > WS-FUZZY-TOTAL-CHARS
               IF FUNCTION UPPER-CASE(
                      WS-FUZZY-STR-1(WS-FUZZY-IDX-1:1)) =
                  FUNCTION UPPER-CASE(
                      WS-FUZZY-STR-2(WS-FUZZY-IDX-1:1))
                   ADD 1 TO WS-FUZZY-MATCH-CHARS
               END-IF
           END-PERFORM

      * CALCULATE PERCENTAGE
           IF WS-FUZZY-TOTAL-CHARS > 0
               COMPUTE WS-FUZZY-RESULT-PCT =
                   (WS-FUZZY-MATCH-CHARS * 30) /
                   WS-FUZZY-TOTAL-CHARS
           END-IF

      * EXACT MATCH GETS FULL SCORE
           IF FUNCTION UPPER-CASE(
                  FUNCTION TRIM(WS-FUZZY-STR-1)) =
              FUNCTION UPPER-CASE(
                  FUNCTION TRIM(WS-FUZZY-STR-2))
               MOVE 30 TO WS-FUZZY-RESULT-PCT
           END-IF.

       6150-EXIT.
           EXIT.

      ****************************************************************
      * 6200-PROCESS-SAM-FILE
      * READS THE SAM (SYSTEM FOR AWARD MANAGEMENT) MONTHLY EXTRACT
      * AND MATCHES AGAINST ACTIVE PROVIDERS. SAM REPLACES THE
      * FORMER EPLS (EXCLUDED PARTIES LIST SYSTEM).
      ****************************************************************
       6200-PROCESS-SAM-FILE.

           DISPLAY 'HCPRVMNT - BEGINNING SAM EXCLUSION PROCESSING'

           SET WS-NOT-END-SAM TO TRUE

           PERFORM UNTIL WS-END-OF-SAM
               READ SAM-EXCLUSION-FILE
                   AT END
                       SET WS-END-OF-SAM TO TRUE
                   NOT AT END
                       ADD 1 TO WS-SAM-RECORDS-READ
                       PERFORM 6300-MATCH-SAM-PROVIDERS
               END-READ
           END-PERFORM

           DISPLAY 'HCPRVMNT - SAM RECORDS READ:     '
                   WS-SAM-RECORDS-READ
           DISPLAY 'HCPRVMNT - SAM MATCHES FOUND:    '
                   WS-SAM-MATCHES-FOUND.

      ****************************************************************
      * 6300-MATCH-SAM-PROVIDERS
      * PERFORMS MATCHING LOGIC FOR SAM EXCLUSION RECORDS AGAINST
      * THE PROVIDER MASTER. USES NPI, TIN, AND NAME MATCHING.
      ****************************************************************
       6300-MATCH-SAM-PROVIDERS.

           INITIALIZE WS-SAM-MATCH-WORK
           SET WS-SAM-NO-MATCH TO TRUE
           MOVE ZERO TO WS-SAM-MATCH-SCORE

      * ATTEMPT 1: EXACT MATCH ON NPI
           IF SAM-NPI NOT = SPACES
               EXEC SQL
                   SELECT PROVIDER_ID, NPI, LAST_NAME,
                          FIRST_NAME, TAX_ID
                   INTO :HV-PROVIDER-ID, :HV-NPI,
                        :HV-LAST-NAME, :HV-FIRST-NAME,
                        :HV-TAX-ID
                   FROM PROVIDER_MASTER
                   WHERE NPI = :SAM-NPI
                     AND STATUS IN ('AC', 'SU')
               END-EXEC

               IF SQLCODE = 0
                   SET WS-SAM-EXACT-MATCH TO TRUE
                   MOVE 100 TO WS-SAM-MATCH-SCORE
                   MOVE 'Y' TO WS-SAM-NPI-MATCH
                   ADD 1 TO WS-SAM-MATCHES-FOUND
                   MOVE OIG-EXCL-TYPE TO WS-OIG-EXCLUSION-CODE
                   PERFORM 6400-EXCLUSION-ACTION-PROCESSING
                   GO TO 6300-EXIT
               END-IF
           END-IF

      * ATTEMPT 2: EXACT MATCH ON SSN/EIN
           IF SAM-SSN-EIN NOT = SPACES
               EXEC SQL
                   SELECT PROVIDER_ID, NPI, LAST_NAME,
                          FIRST_NAME, TAX_ID
                   INTO :HV-PROVIDER-ID, :HV-NPI,
                        :HV-LAST-NAME, :HV-FIRST-NAME,
                        :HV-TAX-ID
                   FROM PROVIDER_MASTER
                   WHERE TAX_ID = :SAM-SSN-EIN
                     AND STATUS IN ('AC', 'SU')
               END-EXEC

               IF SQLCODE = 0
                   SET WS-SAM-EXACT-MATCH TO TRUE
                   MOVE 100 TO WS-SAM-MATCH-SCORE
                   MOVE 'Y' TO WS-SAM-TIN-MATCH
                   ADD 1 TO WS-SAM-MATCHES-FOUND
                   PERFORM 6400-EXCLUSION-ACTION-PROCESSING
                   GO TO 6300-EXIT
               END-IF
           END-IF

      * ATTEMPT 3: FUZZY NAME MATCH
           IF SAM-FIRST-NAME NOT = SPACES AND
              SAM-NAME NOT = SPACES
               EXEC SQL
                   SELECT PROVIDER_ID, NPI,
                          LAST_NAME, FIRST_NAME
                   INTO :HV-PROVIDER-ID, :HV-NPI,
                        :HV-LAST-NAME, :HV-FIRST-NAME
                   FROM PROVIDER_MASTER
                   WHERE UPPER(LAST_NAME) = UPPER(:SAM-NAME)
                     AND UPPER(FIRST_NAME) = UPPER(:SAM-FIRST-NAME)
                     AND PRACTICE_STATE = :SAM-STATE
                     AND STATUS IN ('AC', 'SU')
               END-EXEC

               IF SQLCODE = 0
                   SET WS-SAM-FUZZY-MATCH TO TRUE
                   MOVE 85 TO WS-SAM-MATCH-SCORE
                   ADD 1 TO WS-SAM-MATCHES-FOUND
                   PERFORM 6400-EXCLUSION-ACTION-PROCESSING
               END-IF
           END-IF.

       6300-EXIT.
           EXIT.

      ****************************************************************
      * 6400-EXCLUSION-ACTION-PROCESSING
      * PROCESSES ACTIONS FOR CONFIRMED EXCLUSION MATCHES INCLUDING
      * IMMEDIATE PAYMENT SUSPENSION, MEMBER REASSIGNMENT TRIGGER,
      * CLAIMS REVIEW/RECOUPMENT INITIATION, REGULATORY REPORTING,
      * EXCLUSION DURATION TRACKING, AND REINSTATEMENT MONITORING.
      ****************************************************************
       6400-EXCLUSION-ACTION-PROCESSING.

           ADD 1 TO WS-EXCLUSION-ACTION-CTR

      * 1. IMMEDIATE PAYMENT SUSPENSION
           EXEC SQL
               UPDATE PROVIDER_MASTER
               SET STATUS = 'SU',
                   EFT_STATUS = 'SU',
                   ACCEPTING_NEW = 'N',
                   LAST_UPDATE_DATE = :WS-FORMATTED-DATE,
                   LAST_UPDATE_PGM = :WS-PROGRAM-NAME
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND STATUS = 'AC'
           END-EXEC

           IF SQLCODE NOT = 0 AND SQLCODE NOT = 100
               PERFORM 8100-DATABASE-ERROR
           END-IF

      * 2. INSERT EXCLUSION MATCH RECORD
           EXEC SQL
               INSERT INTO EXCLUSION_MATCH_LOG
               (PROVIDER_ID, NPI,
                MATCH_SOURCE, MATCH_TYPE,
                MATCH_SCORE, EXCLUSION_CODE,
                EXCLUSION_DATE, MATCH_DATE,
                MATCH_STATUS, ACTION_TAKEN,
                PAYMENT_SUSPENDED, MEMBER_REASSIGN_TRIGGERED,
                CLAIMS_REVIEW_INITIATED)
               VALUES
               (:HV-PROVIDER-ID, :HV-NPI,
                'OIG', :WS-OIG-MATCH-TYPE,
                :WS-OIG-MATCH-SCORE, :WS-OIG-EXCLUSION-CODE,
                CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,
                'CF', 'PAYMENT SUSPENSION',
                'Y', 'Y', 'Y')
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF

      * 3. TRIGGER MEMBER REASSIGNMENT
           EXEC SQL
               SELECT COUNT(*)
               INTO :HV-MEMBER-COUNT
               FROM MEMBER_PCP_ASSIGNMENT
               WHERE PROVIDER_ID = :HV-PROVIDER-ID
                 AND STATUS = 'AC'
           END-EXEC

           IF SQLCODE = 0 AND HV-MEMBER-COUNT > 0
               EXEC SQL
                   INSERT INTO MEMBER_REASSIGN_QUEUE
                   (PROVIDER_ID, MEMBER_COUNT,
                    REASON_CODE, EFFECTIVE_DATE,
                    NOTIFY_REQUIRED, QUEUE_DATE,
                    STATUS, CREATED_BY)
                   VALUES
                   (:HV-PROVIDER-ID, :HV-MEMBER-COUNT,
                    'EXC', CURRENT_TIMESTAMP,
                    'Y', CURRENT_TIMESTAMP,
                    'UR', :WS-PROGRAM-NAME)
               END-EXEC
               ADD HV-MEMBER-COUNT TO WS-PCP-REASSIGN-CTR
           END-IF

      * 4. INITIATE CLAIMS REVIEW/RECOUPMENT
           EXEC SQL
               INSERT INTO CLAIMS_REVIEW_QUEUE
               (PROVIDER_ID, REVIEW_REASON,
                REVIEW_PERIOD_START, REVIEW_PERIOD_END,
                QUEUE_DATE, STATUS, PRIORITY)
               VALUES
               (:HV-PROVIDER-ID, 'EXCLUSION MATCH',
                DATEADD(YEAR, -6, CURRENT_DATE),
                CURRENT_DATE,
                CURRENT_TIMESTAMP, 'PD', 'HIGH')
           END-EXEC

           IF SQLCODE NOT = 0
               PERFORM 8100-DATABASE-ERROR
           END-IF

      * 5. REGULATORY REPORTING
           EXEC SQL
               INSERT INTO REGULATORY_REPORT_QUEUE
               (PROVIDER_ID, REPORT_TYPE,
                REPORT_REASON, DUE_DATE,
                STATUS, CREATED_DATE)
               VALUES
               (:HV-PROVIDER-ID, 'EXCL-NOTIFY',
                'OIG/SAM EXCLUSION MATCH',
                DATEADD(DAY, 5, CURRENT_DATE),
                'PD', CURRENT_TIMESTAMP)
           END-EXEC

      * 6. WRITE CRITICAL ERROR
           MOVE 'C' TO ERR-SEVERITY
           MOVE 'EX0001' TO ERR-CODE
           MOVE 'EXCLUSION-MATCH' TO ERR-FIELD-NAME
           MOVE HV-NPI TO ERR-FIELD-VALUE
           MOVE 'EXCLUSION MATCH CONFIRMED - ALL ACTIONS INITIATED'
               TO ERR-MESSAGE
           MOVE '6400-EXCLUSION-ACTION-PROCESSING'
               TO ERR-PARAGRAPH-NAME
           PERFORM 8000-ERROR-HANDLER

      * 7. WRITE AUDIT TRAIL
           MOVE WS-FORMATTED-TIMESTAMP TO AUD-TIMESTAMP
           MOVE HV-PROVIDER-ID TO AUD-PROVIDER-ID
           MOVE HV-NPI TO AUD-NPI
           MOVE 'EXC' TO AUD-ACTION-TYPE
           MOVE ZERO TO AUD-TRANS-SEQ
           MOVE WS-PROGRAM-NAME TO AUD-USER-ID
           MOVE 'BATCH' TO AUD-SOURCE-SYSTEM
           MOVE 'EXCLUSION_MATCH_LOG' TO AUD-TABLE-NAME
           MOVE 'STATUS' TO AUD-FIELD-NAME
           MOVE 'AC' TO AUD-BEFORE-VALUE
           MOVE 'SU' TO AUD-AFTER-VALUE
           MOVE WS-FORMATTED-DATE TO AUD-EFF-DATE
           MOVE '00' TO AUD-RESULT-CODE
           MOVE 'EXCLUSION MATCH - PROVIDER SUSPENDED'
               TO AUD-RESULT-MSG
           WRITE AUDIT-REC
           ADD 1 TO WS-AUDIT-CTR.

       6400-EXIT.
           EXIT.

      ****************************************************************
      * 7000-GENERATE-CREDENTIALING-REPORT
      * GENERATES THE CREDENTIALING STATUS DASHBOARD REPORT SHOWING
      * ALL PROVIDER CREDENTIALING STATUSES, UPCOMING EXPIRATIONS,
      * PENDING VERIFICATIONS, AND RECREDENTIALING DUE DATES.
      ****************************************************************
       7000-GENERATE-CREDENTIALING-REPORT.

           MOVE 'CREDENTIALING STATUS REPORT' TO WS-RPT-TITLE
           MOVE 'PROVIDER CREDENTIALING DASHBOARD'
               TO WS-RPT-SUBTITLE
           MOVE WS-FORMATTED-DATE TO WS-RPT-DATE
           MOVE ZERO TO WS-CRED-RPT-PAGE
           MOVE 99 TO WS-CRED-RPT-LINE

           EXEC SQL
               DECLARE CRED_RPT_CURSOR CURSOR FOR
               SELECT P.PROVIDER_ID, P.NPI,
                      P.LAST_NAME, P.FIRST_NAME,
                      P.SPECIALTY_CODE, P.CRED_STATUS,
                      P.CRED_SCORE, P.CRED_DATE,
                      P.RECRED_DUE_DATE
               FROM PROVIDER_MASTER P
               WHERE P.STATUS IN ('AC', 'SU', 'PR')
               ORDER BY P.CRED_STATUS, P.RECRED_DUE_DATE
           END-EXEC

           EXEC SQL
               OPEN CRED_RPT_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCPRVMNT - CANNOT OPEN CRED RPT CURSOR'
               GO TO 7000-EXIT
           END-IF

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CRED_RPT_CURSOR
                   INTO :HV-PROVIDER-ID, :HV-NPI,
                        :HV-LAST-NAME, :HV-FIRST-NAME,
                        :HV-SPECIALTY-CODE, :HV-CRED-STATUS,
                        :HV-CRED-SCORE, :HV-CRED-DATE,
                        :HV-RECRED-DUE-DATE
               END-EXEC

               IF SQLCODE = 0
      * CHECK IF NEW PAGE NEEDED
                   IF WS-CRED-RPT-LINE >= WS-LINES-PER-PAGE
                       ADD 1 TO WS-CRED-RPT-PAGE
                       MOVE WS-CRED-RPT-PAGE TO WS-RPT-PAGE-NO
                       WRITE CRED-RPT-REC FROM WS-RPT-HEADER-1
                           AFTER ADVANCING TOP-OF-PAGE
                       WRITE CRED-RPT-REC FROM WS-RPT-HEADER-2
                           AFTER ADVANCING 1 LINES
                       MOVE SPACES TO CRED-RPT-REC
                       WRITE CRED-RPT-REC
                           AFTER ADVANCING 1 LINES
                       MOVE 3 TO WS-CRED-RPT-LINE
                   END-IF

      * FORMAT DETAIL LINE
                   INITIALIZE WS-CRED-RPT-DETAIL
                   MOVE HV-PROVIDER-ID TO WS-CR-PROVIDER-ID
                   MOVE HV-NPI TO WS-CR-NPI
                   STRING HV-LAST-NAME DELIMITED BY '  '
                          ', ' DELIMITED BY SIZE
                          HV-FIRST-NAME DELIMITED BY '  '
                       INTO WS-CR-PROVIDER-NAME
                   END-STRING
                   MOVE HV-SPECIALTY-CODE TO WS-CR-SPECIALTY

                   EVALUATE HV-CRED-STATUS
                       WHEN 'FA'
                           MOVE 'FULL APRVL ' TO WS-CR-CRED-STATUS
                       WHEN 'CA'
                           MOVE 'CONDITIONAL ' TO WS-CR-CRED-STATUS
                       WHEN 'DN'
                           MOVE 'DENIED      ' TO WS-CR-CRED-STATUS
                       WHEN 'PD'
                           MOVE 'PENDING     ' TO WS-CR-CRED-STATUS
                       WHEN 'EX'
                           MOVE 'EXPIRED     ' TO WS-CR-CRED-STATUS
                       WHEN OTHER
                           MOVE HV-CRED-STATUS TO WS-CR-CRED-STATUS
                   END-EVALUATE

                   MOVE HV-CRED-SCORE TO WS-CR-CRED-SCORE
                   MOVE HV-CRED-DATE TO WS-CR-CRED-DATE
                   MOVE HV-RECRED-DUE-DATE TO WS-CR-RECRED-DUE

      * CHECK FOR ALERTS
                   IF HV-RECRED-DUE-DATE NOT = SPACES
                       IF HV-RECRED-DUE-DATE <= WS-FORMATTED-DATE
                           MOVE '** OVERDUE' TO WS-CR-ALERT
                       END-IF
                   END-IF

                   IF HV-CRED-STATUS = 'EX'
                       MOVE '** EXPIRED' TO WS-CR-ALERT
                   END-IF

                   WRITE CRED-RPT-REC FROM WS-CRED-RPT-DETAIL
                       AFTER ADVANCING 1 LINES
                   ADD 1 TO WS-CRED-RPT-LINE
               END-IF
           END-PERFORM

           EXEC SQL
               CLOSE CRED_RPT_CURSOR
           END-EXEC

           DISPLAY 'HCPRVMNT - CREDENTIALING REPORT GENERATED'.

       7000-EXIT.
           EXIT.

      ****************************************************************
      * 7100-GENERATE-NETWORK-REPORT
      * GENERATES NETWORK ADEQUACY SUMMARY REPORT SHOWING PROVIDER
      * COUNTS BY SPECIALTY AND GEOGRAPHY, MEMBER-TO-PROVIDER RATIOS,
      * AND IDENTIFIES NETWORK GAPS.
      ****************************************************************
       7100-GENERATE-NETWORK-REPORT.

           MOVE 'NETWORK ADEQUACY REPORT       ' TO WS-RPT-TITLE
           MOVE 'PROVIDER COUNTS BY SPECIALTY/GEO'
               TO WS-RPT-SUBTITLE
           MOVE WS-FORMATTED-DATE TO WS-RPT-DATE
           MOVE ZERO TO WS-NET-RPT-PAGE
           MOVE 99 TO WS-NET-RPT-LINE

           EXEC SQL
               DECLARE NET_RPT_CURSOR CURSOR FOR
               SELECT P.NETWORK_ID,
                      P.SPECIALTY_CODE,
                      P.PRACTICE_COUNTY,
                      COUNT(*) AS PROV_COUNT,
                      Z.URBAN_RURAL_IND,
                      Z.COUNTY_NAME
               FROM PROVIDER_MASTER P
               LEFT JOIN ZIP_COUNTY_MAPPING Z
                   ON P.PRACTICE_COUNTY = Z.COUNTY_FIPS_CODE
               WHERE P.STATUS = 'AC'
                 AND P.CRED_STATUS IN ('FA', 'CA')
               GROUP BY P.NETWORK_ID, P.SPECIALTY_CODE,
                        P.PRACTICE_COUNTY,
                        Z.URBAN_RURAL_IND, Z.COUNTY_NAME
               ORDER BY P.NETWORK_ID, P.SPECIALTY_CODE,
                        P.PRACTICE_COUNTY
           END-EXEC

           EXEC SQL
               OPEN NET_RPT_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCPRVMNT - CANNOT OPEN NETWORK RPT CURSOR'
               GO TO 7100-EXIT
           END-IF

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH NET_RPT_CURSOR
                   INTO :WS-NR-NETWORK-ID,
                        :WS-NET-ADQ-SPECIALTY,
                        :WS-NET-ADQ-COUNTY,
                        :HV-PROVIDER-COUNT,
                        :WS-NET-ADQ-URBAN-RURAL,
                        :WS-ZIP-COUNTY-NAME
               END-EXEC

               IF SQLCODE = 0
                   IF WS-NET-RPT-LINE >= WS-LINES-PER-PAGE
                       ADD 1 TO WS-NET-RPT-PAGE
                       MOVE WS-NET-RPT-PAGE TO WS-RPT-PAGE-NO
                       WRITE NET-RPT-REC FROM WS-RPT-HEADER-1
                           AFTER ADVANCING TOP-OF-PAGE
                       WRITE NET-RPT-REC FROM WS-RPT-HEADER-2
                           AFTER ADVANCING 1 LINES
                       MOVE SPACES TO NET-RPT-REC
                       WRITE NET-RPT-REC
                           AFTER ADVANCING 1 LINES
                       MOVE 3 TO WS-NET-RPT-LINE
                   END-IF

                   INITIALIZE WS-NET-RPT-DETAIL
                   MOVE WS-NR-NETWORK-ID TO WS-NR-NETWORK-ID
                   MOVE WS-NET-ADQ-SPECIALTY TO WS-NR-SPECIALTY
                   MOVE WS-ZIP-COUNTY-NAME TO WS-NR-COUNTY
                   MOVE HV-PROVIDER-COUNT TO WS-NR-PROV-COUNT

      * GET MEMBER COUNT FOR THIS COUNTY
                   EXEC SQL
                       SELECT COUNT(*)
                       INTO :HV-MEMBER-COUNT
                       FROM MEMBER_MASTER
                       WHERE COUNTY_CODE = :WS-NET-ADQ-COUNTY
                         AND MEMBER_STATUS = 'AC'
                   END-EXEC

                   IF SQLCODE = 0
                       MOVE HV-MEMBER-COUNT TO WS-NR-MBR-COUNT
                       IF HV-PROVIDER-COUNT > 0
                           COMPUTE WS-NET-ADQ-RATIO =
                               HV-MEMBER-COUNT / HV-PROVIDER-COUNT
                           MOVE WS-NET-ADQ-RATIO TO WS-NR-RATIO
                       END-IF
                   END-IF

                   IF WS-NET-ADQ-URBAN-RURAL = 'U'
                       MOVE 'URBAN' TO WS-NR-URBAN-RURAL
                       MOVE WS-NET-ADQ-URB-RATIO TO WS-NR-STANDARD
                       IF WS-NET-ADQ-RATIO <=
                          WS-NET-ADQ-URB-RATIO
                           MOVE 'YES' TO WS-NR-MEETS-STD
                           MOVE '   ' TO WS-NR-GAP-IND
                       ELSE
                           MOVE 'NO ' TO WS-NR-MEETS-STD
                           MOVE 'GAP' TO WS-NR-GAP-IND
                       END-IF
                   ELSE
                       MOVE 'RURAL' TO WS-NR-URBAN-RURAL
                       MOVE WS-NET-ADQ-RUR-RATIO TO WS-NR-STANDARD
                       IF WS-NET-ADQ-RATIO <=
                          WS-NET-ADQ-RUR-RATIO
                           MOVE 'YES' TO WS-NR-MEETS-STD
                           MOVE '   ' TO WS-NR-GAP-IND
                       ELSE
                           MOVE 'NO ' TO WS-NR-MEETS-STD
                           MOVE 'GAP' TO WS-NR-GAP-IND
                       END-IF
                   END-IF

                   WRITE NET-RPT-REC FROM WS-NET-RPT-DETAIL
                       AFTER ADVANCING 1 LINES
                   ADD 1 TO WS-NET-RPT-LINE
               END-IF
           END-PERFORM

           EXEC SQL
               CLOSE NET_RPT_CURSOR
           END-EXEC

           DISPLAY 'HCPRVMNT - NETWORK REPORT GENERATED'.

       7100-EXIT.
           EXIT.

      ****************************************************************
      * 7200-GENERATE-SANCTION-REPORT
      * GENERATES THE SANCTIONS/EXCLUSION MATCH REPORT SHOWING
      * ALL OIG AND SAM MATCH RESULTS, MATCH CONFIDENCE SCORES,
      * ACTIONS TAKEN, AND PENDING REVIEWS.
      ****************************************************************
       7200-GENERATE-SANCTION-REPORT.

           MOVE 'SANCTIONS/EXCLUSION REPORT     ' TO WS-RPT-TITLE
           MOVE 'OIG/SAM MATCH RESULTS AND ACTIONS'
               TO WS-RPT-SUBTITLE
           MOVE WS-FORMATTED-DATE TO WS-RPT-DATE
           MOVE ZERO TO WS-SANC-RPT-PAGE
           MOVE 99 TO WS-SANC-RPT-LINE

           EXEC SQL
               DECLARE SANC_RPT_CURSOR CURSOR FOR
               SELECT E.PROVIDER_ID, E.NPI,
                      P.LAST_NAME, P.FIRST_NAME,
                      E.MATCH_SOURCE, E.MATCH_TYPE,
                      E.MATCH_SCORE, E.EXCLUSION_CODE,
                      E.EXCLUSION_DATE, E.ACTION_TAKEN
               FROM EXCLUSION_MATCH_LOG E
               JOIN PROVIDER_MASTER P
                   ON E.PROVIDER_ID = P.PROVIDER_ID
               WHERE E.MATCH_DATE >= DATEADD(MONTH, -1, CURRENT_DATE)
               ORDER BY E.MATCH_SCORE DESC, E.MATCH_DATE
           END-EXEC

           EXEC SQL
               OPEN SANC_RPT_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'HCPRVMNT - CANNOT OPEN SANCTION RPT CURSOR'
               GO TO 7200-EXIT
           END-IF

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH SANC_RPT_CURSOR
                   INTO :HV-PROVIDER-ID, :HV-NPI,
                        :HV-LAST-NAME, :HV-FIRST-NAME,
                        :WS-WORK-STRING-1, :WS-OIG-MATCH-TYPE,
                        :WS-OIG-MATCH-SCORE,
                        :WS-OIG-EXCLUSION-CODE,
                        :WS-WORK-STRING-2,
                        :WS-OIG-EXCLUSION-DESC
               END-EXEC

               IF SQLCODE = 0
                   IF WS-SANC-RPT-LINE >= WS-LINES-PER-PAGE
                       ADD 1 TO WS-SANC-RPT-PAGE
                       MOVE WS-SANC-RPT-PAGE TO WS-RPT-PAGE-NO
                       WRITE NET-RPT-REC FROM WS-RPT-HEADER-1
                           AFTER ADVANCING TOP-OF-PAGE
                       WRITE NET-RPT-REC FROM WS-RPT-HEADER-2
                           AFTER ADVANCING 1 LINES
                       MOVE SPACES TO NET-RPT-REC
                       WRITE NET-RPT-REC
                           AFTER ADVANCING 1 LINES
                       MOVE 3 TO WS-SANC-RPT-LINE
                   END-IF

                   INITIALIZE WS-SANC-RPT-DETAIL
                   MOVE HV-PROVIDER-ID TO WS-SR-PROVIDER-ID
                   MOVE HV-NPI TO WS-SR-NPI
                   STRING HV-LAST-NAME DELIMITED BY '  '
                          ', ' DELIMITED BY SIZE
                          HV-FIRST-NAME DELIMITED BY '  '
                       INTO WS-SR-PROVIDER-NAME
                   END-STRING
                   MOVE WS-WORK-STRING-1(1:5) TO WS-SR-MATCH-SOURCE

                   EVALUATE WS-OIG-MATCH-TYPE
                       WHEN 'E'
                           MOVE 'EXACT ' TO WS-SR-MATCH-TYPE
                       WHEN 'F'
                           MOVE 'FUZZY ' TO WS-SR-MATCH-TYPE
                       WHEN OTHER
                           MOVE WS-OIG-MATCH-TYPE
                               TO WS-SR-MATCH-TYPE
                   END-EVALUATE

                   MOVE WS-OIG-MATCH-SCORE TO WS-SR-MATCH-SCORE
                   MOVE WS-OIG-EXCLUSION-CODE TO WS-SR-EXCL-CODE
                   MOVE WS-WORK-STRING-2(1:10) TO WS-SR-EXCL-DATE
                   MOVE WS-OIG-EXCLUSION-DESC(1:20)
                       TO WS-SR-ACTION-TAKEN

                   WRITE NET-RPT-REC FROM WS-SANC-RPT-DETAIL
                       AFTER ADVANCING 1 LINES
                   ADD 1 TO WS-SANC-RPT-LINE
               END-IF
           END-PERFORM

           EXEC SQL
               CLOSE SANC_RPT_CURSOR
           END-EXEC

           DISPLAY 'HCPRVMNT - SANCTION REPORT GENERATED'.

       7200-EXIT.
           EXIT.

      ****************************************************************
      * 8000-ERROR-HANDLER
      * CENTRAL ERROR HANDLING ROUTINE. FORMATS AND WRITES ERROR
      * RECORDS TO THE ERROR FILE. INCREMENTS APPROPRIATE COUNTERS.
      * ESCALATES CRITICAL ERRORS FOR IMMEDIATE NOTIFICATION.
      ****************************************************************
       8000-ERROR-HANDLER.

           MOVE WS-FORMATTED-TIMESTAMP TO ERR-TIMESTAMP

           IF ERR-PROVIDER-ID = SPACES
               MOVE PTR-PROVIDER-ID TO ERR-PROVIDER-ID
           END-IF

           IF ERR-NPI = SPACES
               MOVE PTR-NPI TO ERR-NPI
           END-IF

           IF ERR-TRANS-TYPE = SPACES
               MOVE PTR-TRANS-TYPE TO ERR-TRANS-TYPE
           END-IF

           IF ERR-TRANS-SEQ = ZERO
               MOVE PTR-TRANS-SEQ-NO TO ERR-TRANS-SEQ
           END-IF

           WRITE ERROR-REC
           ADD 1 TO WS-ERROR-WRITTEN-CTR
           ADD 1 TO WS-TRANS-ERROR-CTR

      * LOG CRITICAL ERRORS TO CONSOLE
           IF ERR-CRITICAL
               DISPLAY 'HCPRVMNT *** CRITICAL ERROR ***'
               DISPLAY 'HCPRVMNT - CODE: ' ERR-CODE
                       ' PROVIDER: ' ERR-PROVIDER-ID
               DISPLAY 'HCPRVMNT - MSG:  ' ERR-MESSAGE
           END-IF

      * RESET ERROR RECORD FOR NEXT USE
           INITIALIZE ERROR-REC.

      ****************************************************************
      * 8100-DATABASE-ERROR
      * HANDLES DATABASE (SYBASE) ERRORS. LOGS SQLCODE, SQLERRM,
      * AND THE FAILING STATEMENT CONTEXT. DETERMINES IF THE ERROR
      * IS RECOVERABLE OR FATAL.
      ****************************************************************
       8100-DATABASE-ERROR.

           MOVE SQLCODE TO WS-SQLCODE-DISPLAY

           MOVE 'E' TO ERR-SEVERITY
           MOVE 'DB0001' TO ERR-CODE
           MOVE 'SQLCODE' TO ERR-FIELD-NAME
           MOVE WS-SQLCODE-DISPLAY TO ERR-FIELD-VALUE

           EVALUATE TRUE
               WHEN SQLCODE = -803
                   MOVE 'DUPLICATE KEY - RECORD ALREADY EXISTS'
                       TO ERR-MESSAGE
               WHEN SQLCODE = -805
                   MOVE 'PACKAGE NOT FOUND - BIND REQUIRED'
                       TO ERR-MESSAGE
                   MOVE 'Y' TO WS-FATAL-ERROR-SW
               WHEN SQLCODE = -811
                   MOVE 'MULTIPLE ROWS RETURNED - EXPECTED ONE'
                       TO ERR-MESSAGE
               WHEN SQLCODE = -818
                   MOVE 'TIMESTAMP MISMATCH - REBIND REQUIRED'
                       TO ERR-MESSAGE
                   MOVE 'Y' TO WS-FATAL-ERROR-SW
               WHEN SQLCODE = -904
                   MOVE 'RESOURCE UNAVAILABLE - RETRY LATER'
                       TO ERR-MESSAGE
               WHEN SQLCODE = -911
                   MOVE 'DEADLOCK OR TIMEOUT - ROLLBACK OCCURRED'
                       TO ERR-MESSAGE
               WHEN SQLCODE = -913
                   MOVE 'DEADLOCK - TRANSACTION ROLLED BACK'
                       TO ERR-MESSAGE
               WHEN SQLCODE = -922
                   MOVE 'AUTHORIZATION FAILURE'
                       TO ERR-MESSAGE
                   MOVE 'Y' TO WS-FATAL-ERROR-SW
               WHEN SQLCODE = -923
                   MOVE 'DATABASE CONNECTION TERMINATED'
                       TO ERR-MESSAGE
                   MOVE 'Y' TO WS-FATAL-ERROR-SW
               WHEN SQLCODE = -924
                   MOVE 'DATABASE MANAGER NOT ACTIVE'
                       TO ERR-MESSAGE
                   MOVE 'Y' TO WS-FATAL-ERROR-SW
               WHEN SQLCODE = 100
                   MOVE 'ROW NOT FOUND' TO ERR-MESSAGE
               WHEN OTHER
                   STRING 'DATABASE ERROR SQLCODE=' DELIMITED BY SIZE
                          WS-SQLCODE-DISPLAY DELIMITED BY SIZE
                       INTO ERR-MESSAGE
                   END-STRING
           END-EVALUATE

           MOVE '8100-DATABASE-ERROR' TO ERR-PARAGRAPH-NAME
           PERFORM 8000-ERROR-HANDLER

           SET WS-DB-ERROR TO TRUE

           IF WS-FATAL-ERROR
               DISPLAY 'HCPRVMNT *** FATAL DATABASE ERROR ***'
               DISPLAY 'HCPRVMNT - SQLCODE=' WS-SQLCODE-DISPLAY
           END-IF.

      ****************************************************************
      * 9000-TERMINATION
      * CLOSES ALL FILES, DISCONNECTS FROM DATABASE, DISPLAYS
      * FINAL PROCESSING STATISTICS, AND SETS RETURN CODE.
      ****************************************************************
       9000-TERMINATION.

           DISPLAY '================================================='
           DISPLAY 'HCPRVMNT - PROVIDER MAINTENANCE BATCH COMPLETE'
           DISPLAY '================================================='
           DISPLAY ' '
           DISPLAY 'PROCESSING STATISTICS:'
           DISPLAY '---------------------'
           DISPLAY 'TRANSACTIONS READ:          ' WS-TRANS-READ-CTR
           DISPLAY 'TRANSACTIONS PROCESSED:     '
                   WS-TRANS-PROCESSED-CTR
           DISPLAY 'TRANSACTIONS IN ERROR:      ' WS-TRANS-ERROR-CTR
           DISPLAY ' '
           DISPLAY 'ENROLLMENT COUNTS:'
           DISPLAY '  NEW ENROLLMENTS:          ' WS-ENROLL-CTR
           DISPLAY '  DEMOGRAPHIC UPDATES:      ' WS-DEMOG-UPD-CTR
           DISPLAY '  TERMINATIONS:             ' WS-TERM-CTR
           DISPLAY '  REACTIVATIONS:            ' WS-REACT-CTR
           DISPLAY '  ADDRESS CHANGES:          ' WS-ADDR-CHANGE-CTR
           DISPLAY '  SPECIALTY CHANGES:        ' WS-SPEC-CHANGE-CTR
           DISPLAY '  CONTRACT CHANGES:         '
                   WS-CONTRACT-CHANGE-CTR
           DISPLAY '  RETROACTIVE CHANGES:      ' WS-RETRO-CHANGE-CTR
           DISPLAY ' '
           DISPLAY 'CREDENTIALING COUNTS:'
           DISPLAY '  TOTAL CREDENTIALED:       ' WS-CRED-CTR
           DISPLAY '  FULL APPROVAL:            ' WS-CRED-PASSED-CTR
           DISPLAY '  CONDITIONAL:              '
                   WS-CRED-CONDITIONAL-CTR
           DISPLAY '  DENIED:                   ' WS-CRED-FAILED-CTR
           DISPLAY '  RECRED DUE:               ' WS-RECRED-DUE-CTR
           DISPLAY '  DELEGATED CREDENTIALING:  '
                   WS-DELEGATED-CRED-CTR
           DISPLAY ' '
           DISPLAY 'NETWORK COUNTS:'
           DISPLAY '  NETWORK ASSIGNMENTS:      ' WS-NET-CTR
           DISPLAY '  MEETS ADEQUACY STD:       ' WS-NET-ADEQUATE-CTR
           DISPLAY '  ADEQUACY GAPS:            ' WS-NET-GAPS-CTR
           DISPLAY '  PCP REASSIGNMENTS:        ' WS-PCP-REASSIGN-CTR
           DISPLAY ' '
           DISPLAY 'PAYMENT COUNTS:'
           DISPLAY '  PAYMENT SETUPS:           ' WS-PAY-CTR
           DISPLAY '  EFT SETUPS:               ' WS-EFT-SETUP-CTR
           DISPLAY '  PRENOTES GENERATED:       ' WS-PRENOTE-CTR
           DISPLAY '  1099 TRIGGERS:            ' WS-1099-TRIGGER-CTR
           DISPLAY ' '
           DISPLAY 'EXCLUSION COUNTS:'
           DISPLAY '  OIG RECORDS READ:         ' WS-OIG-RECORDS-READ
           DISPLAY '  OIG MATCHES FOUND:        ' WS-OIG-MATCHES-FOUND
           DISPLAY '  OIG EXACT MATCHES:        ' WS-OIG-EXACT-MATCHES
           DISPLAY '  OIG FUZZY MATCHES:        ' WS-OIG-FUZZY-MATCHES
           DISPLAY '  SAM RECORDS READ:         ' WS-SAM-RECORDS-READ
           DISPLAY '  SAM MATCHES FOUND:        ' WS-SAM-MATCHES-FOUND
           DISPLAY '  EXCLUSION ACTIONS:        '
                   WS-EXCLUSION-ACTION-CTR
           DISPLAY ' '
           DISPLAY 'OUTPUT COUNTS:'
           DISPLAY '  MASTER RECORDS WRITTEN:   '
                   WS-MASTER-WRITTEN-CTR
           DISPLAY '  ERROR RECORDS WRITTEN:    '
                   WS-ERROR-WRITTEN-CTR
           DISPLAY '  AUDIT RECORDS WRITTEN:    ' WS-AUDIT-CTR
           DISPLAY ' '

      * COMMIT ANY PENDING DATABASE CHANGES
           IF WS-NO-FATAL-ERROR
               EXEC SQL
                   COMMIT WORK
               END-EXEC

               IF SQLCODE NOT = 0
                   DISPLAY 'HCPRVMNT - WARNING: COMMIT FAILED '
                           'SQLCODE=' SQLCODE
               ELSE
                   DISPLAY 'HCPRVMNT - DATABASE CHANGES COMMITTED'
               END-IF
           ELSE
               EXEC SQL
                   ROLLBACK WORK
               END-EXEC
               DISPLAY 'HCPRVMNT - DATABASE CHANGES ROLLED BACK '
                       'DUE TO FATAL ERROR'
           END-IF

      * DISCONNECT FROM DATABASE
           EXEC SQL
               DISCONNECT CURRENT
           END-EXEC

      * CLOSE ALL FILES
           CLOSE PROVIDER-TRANS-FILE
           CLOSE OIG-EXCLUSION-FILE
           CLOSE SAM-EXCLUSION-FILE
           CLOSE PROVIDER-MASTER-FILE
           CLOSE CREDENTIALING-REPORT
           CLOSE NETWORK-REPORT-FILE
           CLOSE PROVIDER-PAYMENT-FILE
           CLOSE ERROR-FILE
           CLOSE AUDIT-TRAIL-FILE

      * SET RETURN CODE
           IF WS-FATAL-ERROR
               MOVE 16 TO WS-RETURN-CODE
               DISPLAY 'HCPRVMNT - RETURN CODE: 16 (FATAL ERROR)'
           ELSE
               IF WS-TRANS-ERROR-CTR > ZERO
                   MOVE 4 TO WS-RETURN-CODE
                   DISPLAY 'HCPRVMNT - RETURN CODE: 4 (ERRORS FOUND)'
               ELSE
                   MOVE ZERO TO WS-RETURN-CODE
                   DISPLAY 'HCPRVMNT - RETURN CODE: 0 (SUCCESS)'
               END-IF
           END-IF

           MOVE WS-RETURN-CODE TO RETURN-CODE

           DISPLAY 'HCPRVMNT - END OF PROGRAM'.

       9000-EXIT.
           EXIT.
