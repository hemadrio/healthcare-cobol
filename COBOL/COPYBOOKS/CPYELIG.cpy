      *================================================================*
      * COPYBOOK: CPYELIG                                              *
      * DESCRIPTION: ELIGIBILITY/ENROLLMENT RECORD LAYOUT              *
      * SYSTEM: HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)             *
      * AUTHOR: SYSTEMS DEVELOPMENT GROUP                              *
      * DATE WRITTEN: 1996-01-10                                       *
      * DATE MODIFIED: 2014-09-15 - ACA MARKETPLACE UPDATES            *
      *================================================================*

       01  WS-ELIGIBILITY-REC.
           05  WS-ELG-KEY.
               10  WS-ELG-MEMBER-ID          PIC X(20).
               10  WS-ELG-SEQ-NO             PIC 9(03).
           05  WS-ELG-PLAN-INFO.
               10  WS-ELG-PLAN-CD            PIC X(08).
               10  WS-ELG-PLAN-NAME          PIC X(40).
               10  WS-ELG-PLAN-TYPE          PIC X(03).
                   88  WS-ELG-HMO            VALUE 'HMO'.
                   88  WS-ELG-PPO            VALUE 'PPO'.
                   88  WS-ELG-POS            VALUE 'POS'.
                   88  WS-ELG-EPO            VALUE 'EPO'.
                   88  WS-ELG-HDHP           VALUE 'HDH'.
                   88  WS-ELG-INDEMNITY      VALUE 'IND'.
                   88  WS-ELG-MEDICARE-ADV   VALUE 'MAD'.
                   88  WS-ELG-MEDICAID-MGD   VALUE 'MMC'.
               10  WS-ELG-PRODUCT-CD         PIC X(06).
               10  WS-ELG-GROUP-NO           PIC X(15).
               10  WS-ELG-GROUP-NAME         PIC X(40).
               10  WS-ELG-SUB-GROUP          PIC X(06).
               10  WS-ELG-CLASS-CD           PIC X(04).
               10  WS-ELG-DIVISION-CD        PIC X(04).
               10  WS-ELG-NETWORK-CD         PIC X(06).
           05  WS-ELG-MEMBER-INFO.
               10  WS-ELG-MBR-LAST-NAME     PIC X(35).
               10  WS-ELG-MBR-FIRST-NAME    PIC X(25).
               10  WS-ELG-MBR-DOB           PIC 9(08).
               10  WS-ELG-MBR-GENDER        PIC X(01).
               10  WS-ELG-MBR-RELATION      PIC X(02).
                   88  WS-ELG-SUBSCRIBER     VALUE '18'.
                   88  WS-ELG-SPOUSE         VALUE '01'.
                   88  WS-ELG-CHILD          VALUE '19'.
                   88  WS-ELG-DEPENDENT      VALUE '20'.
               10  WS-ELG-SUBSCRIBER-ID      PIC X(20).
               10  WS-ELG-PERSON-NO          PIC X(02).
               10  WS-ELG-PCP-NPI            PIC X(10).
               10  WS-ELG-PCP-NAME           PIC X(50).
           05  WS-ELG-COVERAGE-INFO.
               10  WS-ELG-EFF-DT             PIC 9(08).
               10  WS-ELG-TERM-DT            PIC 9(08).
               10  WS-ELG-ENROLL-DT          PIC 9(08).
               10  WS-ELG-DISENROLL-DT       PIC 9(08).
               10  WS-ELG-TERM-REASON        PIC X(03).
               10  WS-ELG-COV-STATUS         PIC X(01).
                   88  WS-ELG-ACTIVE         VALUE 'A'.
                   88  WS-ELG-TERMED         VALUE 'T'.
                   88  WS-ELG-COBRA          VALUE 'C'.
                   88  WS-ELG-PENDING        VALUE 'P'.
                   88  WS-ELG-SUSPENDED      VALUE 'S'.
               10  WS-ELG-COV-LEVEL          PIC X(02).
                   88  WS-ELG-EE-ONLY        VALUE 'EO'.
                   88  WS-ELG-EE-SPOUSE      VALUE 'ES'.
                   88  WS-ELG-EE-CHILD       VALUE 'EC'.
                   88  WS-ELG-FAMILY         VALUE 'FA'.
           05  WS-ELG-BENEFIT-INFO.
               10  WS-ELG-DEDUCTIBLE-IND    PIC S9(07)V99 COMP-3.
               10  WS-ELG-DEDUCTIBLE-FAM    PIC S9(07)V99 COMP-3.
               10  WS-ELG-DEDUCT-MET-IND    PIC S9(07)V99 COMP-3.
               10  WS-ELG-DEDUCT-MET-FAM    PIC S9(07)V99 COMP-3.
               10  WS-ELG-OOP-MAX-IND       PIC S9(07)V99 COMP-3.
               10  WS-ELG-OOP-MAX-FAM       PIC S9(07)V99 COMP-3.
               10  WS-ELG-OOP-MET-IND       PIC S9(07)V99 COMP-3.
               10  WS-ELG-OOP-MET-FAM       PIC S9(07)V99 COMP-3.
               10  WS-ELG-COPAY-PCP          PIC S9(05)V99 COMP-3.
               10  WS-ELG-COPAY-SPEC        PIC S9(05)V99 COMP-3.
               10  WS-ELG-COPAY-ER          PIC S9(05)V99 COMP-3.
               10  WS-ELG-COPAY-URGENT      PIC S9(05)V99 COMP-3.
               10  WS-ELG-COPAY-RX-GEN      PIC S9(05)V99 COMP-3.
               10  WS-ELG-COPAY-RX-BRAND    PIC S9(05)V99 COMP-3.
               10  WS-ELG-COPAY-RX-SPEC     PIC S9(05)V99 COMP-3.
               10  WS-ELG-COINSUR-IN-NET    PIC S9(03)V99 COMP-3.
               10  WS-ELG-COINSUR-OON       PIC S9(03)V99 COMP-3.
               10  WS-ELG-LIFETIME-MAX       PIC S9(09)V99 COMP-3.
               10  WS-ELG-LIFETIME-USED      PIC S9(09)V99 COMP-3.
               10  WS-ELG-ANNUAL-MAX         PIC S9(09)V99 COMP-3.
               10  WS-ELG-ANNUAL-USED        PIC S9(09)V99 COMP-3.
           05  WS-ELG-COB-INFO.
               10  WS-ELG-COB-FLAG           PIC X(01).
                   88  WS-ELG-HAS-COB        VALUE 'Y'.
               10  WS-ELG-COB-PAYER-CD       PIC X(08).
               10  WS-ELG-COB-PAYER-NAME     PIC X(40).
               10  WS-ELG-COB-MEMBER-ID      PIC X(20).
               10  WS-ELG-COB-GROUP-NO       PIC X(15).
               10  WS-ELG-COB-SEQ            PIC 9(01).
               10  WS-ELG-COB-EFF-DT         PIC 9(08).
               10  WS-ELG-COB-TERM-DT        PIC 9(08).
           05  WS-ELG-ACA-INFO.
               10  WS-ELG-EXCHANGE-IND       PIC X(01).
                   88  WS-ELG-EXCHANGE-YES   VALUE 'Y'.
               10  WS-ELG-METAL-TIER         PIC X(02).
                   88  WS-ELG-BRONZE         VALUE 'BR'.
                   88  WS-ELG-SILVER         VALUE 'SL'.
                   88  WS-ELG-GOLD           VALUE 'GD'.
                   88  WS-ELG-PLATINUM       VALUE 'PL'.
                   88  WS-ELG-CATASTROPHIC   VALUE 'CT'.
               10  WS-ELG-APTC-AMT           PIC S9(07)V99 COMP-3.
               10  WS-ELG-CSR-VARIANT        PIC X(02).
               10  WS-ELG-ESSENTIAL-HLTH     PIC X(01).
                   88  WS-ELG-EHB-YES        VALUE 'Y'.
           05  WS-ELG-AUDIT-FIELDS.
               10  WS-ELG-CREATE-DT          PIC 9(08).
               10  WS-ELG-CREATE-USER        PIC X(08).
               10  WS-ELG-LAST-UPD-DT        PIC 9(08).
               10  WS-ELG-LAST-UPD-USER      PIC X(08).
