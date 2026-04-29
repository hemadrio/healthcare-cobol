      *================================================================*
      * COPYBOOK: CPYPATIN                                             *
      * DESCRIPTION: PATIENT MASTER INFORMATION RECORD LAYOUT          *
      * SYSTEM: HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)             *
      * AUTHOR: SYSTEMS DEVELOPMENT GROUP                              *
      * DATE WRITTEN: 1994-03-15                                       *
      * DATE MODIFIED: 2002-11-20 - ADDED HIPAA FIELDS                 *
      *                2008-06-01 - ADDED MEDICARE PART D FIELDS       *
      *                2014-09-15 - ACA COMPLIANCE UPDATES             *
      *================================================================*

       01  WS-PATIENT-MASTER-REC.
           05  WS-PAT-KEY.
               10  WS-PAT-MRN                 PIC X(12).
               10  WS-PAT-FACILITY-CD         PIC X(04).
           05  WS-PAT-DEMOGRAPHICS.
               10  WS-PAT-LAST-NAME           PIC X(35).
               10  WS-PAT-FIRST-NAME          PIC X(25).
               10  WS-PAT-MIDDLE-INIT         PIC X(01).
               10  WS-PAT-NAME-SUFFIX         PIC X(04).
               10  WS-PAT-DOB                 PIC 9(08).
               10  WS-PAT-DOB-R REDEFINES WS-PAT-DOB.
                   15  WS-PAT-DOB-CC          PIC 9(02).
                   15  WS-PAT-DOB-YY          PIC 9(02).
                   15  WS-PAT-DOB-MM          PIC 9(02).
                   15  WS-PAT-DOB-DD          PIC 9(02).
               10  WS-PAT-GENDER              PIC X(01).
                   88  WS-PAT-MALE            VALUE 'M'.
                   88  WS-PAT-FEMALE          VALUE 'F'.
                   88  WS-PAT-UNKNOWN-GEN     VALUE 'U'.
               10  WS-PAT-SSN                 PIC X(09).
               10  WS-PAT-SSN-R REDEFINES WS-PAT-SSN.
                   15  WS-PAT-SSN-AREA        PIC X(03).
                   15  WS-PAT-SSN-GROUP       PIC X(02).
                   15  WS-PAT-SSN-SERIAL      PIC X(04).
               10  WS-PAT-MARITAL-STATUS      PIC X(01).
                   88  WS-PAT-SINGLE          VALUE 'S'.
                   88  WS-PAT-MARRIED         VALUE 'M'.
                   88  WS-PAT-DIVORCED        VALUE 'D'.
                   88  WS-PAT-WIDOWED         VALUE 'W'.
                   88  WS-PAT-SEPARATED       VALUE 'P'.
               10  WS-PAT-RACE-CD             PIC X(02).
               10  WS-PAT-ETHNICITY-CD        PIC X(02).
               10  WS-PAT-LANGUAGE-CD         PIC X(03).
           05  WS-PAT-ADDRESS-INFO.
               10  WS-PAT-ADDR-LINE-1         PIC X(40).
               10  WS-PAT-ADDR-LINE-2         PIC X(40).
               10  WS-PAT-CITY                PIC X(30).
               10  WS-PAT-STATE               PIC X(02).
               10  WS-PAT-ZIP                 PIC X(09).
               10  WS-PAT-ZIP-R REDEFINES WS-PAT-ZIP.
                   15  WS-PAT-ZIP5            PIC X(05).
                   15  WS-PAT-ZIP4            PIC X(04).
               10  WS-PAT-COUNTY-CD           PIC X(05).
               10  WS-PAT-COUNTRY-CD          PIC X(03).
               10  WS-PAT-HOME-PHONE          PIC X(10).
               10  WS-PAT-WORK-PHONE          PIC X(10).
               10  WS-PAT-CELL-PHONE          PIC X(10).
               10  WS-PAT-EMAIL               PIC X(60).
           05  WS-PAT-INSURANCE-INFO.
               10  WS-PAT-PRI-INS.
                   15  WS-PAT-PRI-PLAN-CD     PIC X(08).
                   15  WS-PAT-PRI-GROUP-NO    PIC X(15).
                   15  WS-PAT-PRI-MEMBER-ID   PIC X(20).
                   15  WS-PAT-PRI-EFF-DT      PIC 9(08).
                   15  WS-PAT-PRI-TERM-DT     PIC 9(08).
                   15  WS-PAT-PRI-COPAY-AMT   PIC S9(05)V99 COMP-3.
                   15  WS-PAT-PRI-DEDUCT-AMT  PIC S9(07)V99 COMP-3.
                   15  WS-PAT-PRI-DEDUCT-MET  PIC S9(07)V99 COMP-3.
                   15  WS-PAT-PRI-OOP-MAX     PIC S9(07)V99 COMP-3.
                   15  WS-PAT-PRI-OOP-MET     PIC S9(07)V99 COMP-3.
                   15  WS-PAT-PRI-COINSUR-PCT PIC S9(03)V99 COMP-3.
               10  WS-PAT-SEC-INS.
                   15  WS-PAT-SEC-PLAN-CD     PIC X(08).
                   15  WS-PAT-SEC-GROUP-NO    PIC X(15).
                   15  WS-PAT-SEC-MEMBER-ID   PIC X(20).
                   15  WS-PAT-SEC-EFF-DT      PIC 9(08).
                   15  WS-PAT-SEC-TERM-DT     PIC 9(08).
                   15  WS-PAT-SEC-COPAY-AMT   PIC S9(05)V99 COMP-3.
                   15  WS-PAT-SEC-DEDUCT-AMT  PIC S9(07)V99 COMP-3.
                   15  WS-PAT-SEC-DEDUCT-MET  PIC S9(07)V99 COMP-3.
                   15  WS-PAT-SEC-OOP-MAX     PIC S9(07)V99 COMP-3.
                   15  WS-PAT-SEC-OOP-MET     PIC S9(07)V99 COMP-3.
                   15  WS-PAT-SEC-COINSUR-PCT PIC S9(03)V99 COMP-3.
               10  WS-PAT-TER-INS.
                   15  WS-PAT-TER-PLAN-CD     PIC X(08).
                   15  WS-PAT-TER-GROUP-NO    PIC X(15).
                   15  WS-PAT-TER-MEMBER-ID   PIC X(20).
                   15  WS-PAT-TER-EFF-DT      PIC 9(08).
                   15  WS-PAT-TER-TERM-DT     PIC 9(08).
           05  WS-PAT-MEDICARE-INFO.
               10  WS-PAT-MCARE-HIC-NO       PIC X(12).
               10  WS-PAT-MCARE-MBI-NO       PIC X(11).
               10  WS-PAT-MCARE-PART-A       PIC X(01).
                   88  WS-PAT-HAS-PART-A     VALUE 'Y'.
               10  WS-PAT-MCARE-PART-B       PIC X(01).
                   88  WS-PAT-HAS-PART-B     VALUE 'Y'.
               10  WS-PAT-MCARE-PART-C       PIC X(01).
                   88  WS-PAT-HAS-PART-C     VALUE 'Y'.
               10  WS-PAT-MCARE-PART-D       PIC X(01).
                   88  WS-PAT-HAS-PART-D     VALUE 'Y'.
               10  WS-PAT-MCARE-PART-A-EFF   PIC 9(08).
               10  WS-PAT-MCARE-PART-B-EFF   PIC 9(08).
               10  WS-PAT-MCARE-MSP-CD       PIC X(02).
           05  WS-PAT-MEDICAID-INFO.
               10  WS-PAT-MCAID-ID           PIC X(15).
               10  WS-PAT-MCAID-STATE        PIC X(02).
               10  WS-PAT-MCAID-EFF-DT       PIC 9(08).
               10  WS-PAT-MCAID-TERM-DT      PIC 9(08).
               10  WS-PAT-MCAID-AID-CAT      PIC X(03).
               10  WS-PAT-DUAL-ELIGIBLE       PIC X(01).
                   88  WS-PAT-IS-DUAL-ELIG   VALUE 'Y'.
           05  WS-PAT-CLINICAL-FLAGS.
               10  WS-PAT-ALLERGIES-FLAG      PIC X(01).
                   88  WS-PAT-HAS-ALLERGIES  VALUE 'Y'.
               10  WS-PAT-DNR-FLAG            PIC X(01).
                   88  WS-PAT-HAS-DNR        VALUE 'Y'.
               10  WS-PAT-ADVANCE-DIR-FL     PIC X(01).
                   88  WS-PAT-HAS-ADV-DIR    VALUE 'Y'.
               10  WS-PAT-ORGAN-DONOR-FL     PIC X(01).
                   88  WS-PAT-IS-DONOR       VALUE 'Y'.
               10  WS-PAT-DISABILITY-CD      PIC X(02).
               10  WS-PAT-CHRONIC-COND-CNT   PIC 9(03).
               10  WS-PAT-RISK-SCORE         PIC S9(03)V9999 COMP-3.
           05  WS-PAT-EMPLOYMENT-INFO.
               10  WS-PAT-EMPLOYER-NAME      PIC X(40).
               10  WS-PAT-EMPLOYER-ADDR      PIC X(60).
               10  WS-PAT-EMPLOYMENT-STAT    PIC X(01).
                   88  WS-PAT-EMPLOYED       VALUE 'E'.
                   88  WS-PAT-RETIRED        VALUE 'R'.
                   88  WS-PAT-UNEMPLOYED     VALUE 'U'.
                   88  WS-PAT-STUDENT        VALUE 'S'.
                   88  WS-PAT-SELF-EMPLOYED  VALUE 'I'.
           05  WS-PAT-GUARANTOR-INFO.
               10  WS-PAT-GUAR-LAST-NAME    PIC X(35).
               10  WS-PAT-GUAR-FIRST-NAME   PIC X(25).
               10  WS-PAT-GUAR-RELATION     PIC X(02).
               10  WS-PAT-GUAR-SSN          PIC X(09).
               10  WS-PAT-GUAR-DOB          PIC 9(08).
               10  WS-PAT-GUAR-ADDR         PIC X(80).
               10  WS-PAT-GUAR-PHONE        PIC X(10).
           05  WS-PAT-EMERGENCY-CONTACT.
               10  WS-PAT-EMERG-NAME        PIC X(50).
               10  WS-PAT-EMERG-RELATION    PIC X(02).
               10  WS-PAT-EMERG-PHONE       PIC X(10).
               10  WS-PAT-EMERG-ALT-PHONE   PIC X(10).
           05  WS-PAT-AUDIT-FIELDS.
               10  WS-PAT-CREATE-DT          PIC 9(08).
               10  WS-PAT-CREATE-TM          PIC 9(06).
               10  WS-PAT-CREATE-USER        PIC X(08).
               10  WS-PAT-LAST-UPD-DT        PIC 9(08).
               10  WS-PAT-LAST-UPD-TM        PIC 9(06).
               10  WS-PAT-LAST-UPD-USER      PIC X(08).
               10  WS-PAT-RECORD-STATUS      PIC X(01).
                   88  WS-PAT-ACTIVE         VALUE 'A'.
                   88  WS-PAT-INACTIVE       VALUE 'I'.
                   88  WS-PAT-DECEASED       VALUE 'D'.
                   88  WS-PAT-MERGED         VALUE 'M'.
               10  WS-PAT-MERGED-TO-MRN      PIC X(12).
               10  WS-PAT-HIPAA-CONSENT      PIC X(01).
                   88  WS-PAT-HIPAA-YES      VALUE 'Y'.
                   88  WS-PAT-HIPAA-NO       VALUE 'N'.
               10  WS-PAT-CONSENT-DT         PIC 9(08).
