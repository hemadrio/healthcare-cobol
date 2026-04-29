      *================================================================*
      * COPYBOOK: CPYPROVD                                             *
      * DESCRIPTION: PROVIDER MASTER RECORD LAYOUT                     *
      * SYSTEM: HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)             *
      * AUTHOR: SYSTEMS DEVELOPMENT GROUP                              *
      * DATE WRITTEN: 1994-03-15                                       *
      * DATE MODIFIED: 2007-05-23 - NPI IMPLEMENTATION                 *
      *================================================================*

       01  WS-PROVIDER-MASTER-REC.
           05  WS-PRV-KEY.
               10  WS-PRV-NPI                PIC X(10).
           05  WS-PRV-IDENTIFIERS.
               10  WS-PRV-TAX-ID             PIC X(09).
               10  WS-PRV-TAX-ID-TYPE        PIC X(01).
                   88  WS-PRV-TID-EIN        VALUE 'E'.
                   88  WS-PRV-TID-SSN        VALUE 'S'.
               10  WS-PRV-LEGACY-ID          PIC X(10).
               10  WS-PRV-UPIN               PIC X(06).
               10  WS-PRV-LICENSE-NO         PIC X(15).
               10  WS-PRV-LICENSE-STATE      PIC X(02).
               10  WS-PRV-DEA-NO             PIC X(09).
               10  WS-PRV-MEDICARE-ID        PIC X(10).
               10  WS-PRV-MEDICAID-ID        PIC X(10).
               10  WS-PRV-MEDICAID-STATE     PIC X(02).
           05  WS-PRV-DEMOGRAPHICS.
               10  WS-PRV-ENTITY-TYPE        PIC X(01).
                   88  WS-PRV-INDIVIDUAL     VALUE '1'.
                   88  WS-PRV-ORGANIZATION   VALUE '2'.
               10  WS-PRV-LAST-NAME          PIC X(35).
               10  WS-PRV-FIRST-NAME         PIC X(25).
               10  WS-PRV-MIDDLE-INIT        PIC X(01).
               10  WS-PRV-CREDENTIALS        PIC X(10).
               10  WS-PRV-ORG-NAME           PIC X(60).
               10  WS-PRV-TAXONOMY-CD        PIC X(10).
               10  WS-PRV-SPECIALTY-CD       PIC X(03).
               10  WS-PRV-SPECIALTY-DESC     PIC X(50).
               10  WS-PRV-BOARD-CERT         PIC X(01).
                   88  WS-PRV-IS-BOARD-CERT  VALUE 'Y'.
               10  WS-PRV-GENDER             PIC X(01).
               10  WS-PRV-DOB                PIC 9(08).
           05  WS-PRV-ADDRESS-INFO.
               10  WS-PRV-PRACTICE-ADDR.
                   15  WS-PRV-PRA-LINE-1     PIC X(40).
                   15  WS-PRV-PRA-LINE-2     PIC X(40).
                   15  WS-PRV-PRA-CITY       PIC X(30).
                   15  WS-PRV-PRA-STATE      PIC X(02).
                   15  WS-PRV-PRA-ZIP        PIC X(09).
                   15  WS-PRV-PRA-PHONE      PIC X(10).
                   15  WS-PRV-PRA-FAX        PIC X(10).
               10  WS-PRV-BILLING-ADDR.
                   15  WS-PRV-BIL-LINE-1     PIC X(40).
                   15  WS-PRV-BIL-LINE-2     PIC X(40).
                   15  WS-PRV-BIL-CITY       PIC X(30).
                   15  WS-PRV-BIL-STATE      PIC X(02).
                   15  WS-PRV-BIL-ZIP        PIC X(09).
           05  WS-PRV-NETWORK-INFO.
               10  WS-PRV-PAR-STATUS         PIC X(01).
                   88  WS-PRV-PAR            VALUE 'P'.
                   88  WS-PRV-NONPAR         VALUE 'N'.
               10  WS-PRV-NETWORK-CD         PIC X(06).
               10  WS-PRV-CONTRACT-CD        PIC X(10).
               10  WS-PRV-CONTRACT-EFF-DT    PIC 9(08).
               10  WS-PRV-CONTRACT-TERM-DT   PIC 9(08).
               10  WS-PRV-CREDNTL-STATUS     PIC X(01).
                   88  WS-PRV-CREDENTIALED   VALUE 'C'.
                   88  WS-PRV-PROVISIONAL    VALUE 'P'.
                   88  WS-PRV-PENDING-CRED   VALUE 'G'.
                   88  WS-PRV-NOT-CRED       VALUE 'N'.
               10  WS-PRV-CREDNTL-DT         PIC 9(08).
               10  WS-PRV-RECREDNTL-DT       PIC 9(08).
               10  WS-PRV-ACCEPT-NEW-PAT     PIC X(01).
                   88  WS-PRV-ACCEPTING      VALUE 'Y'.
               10  WS-PRV-PCP-FLAG           PIC X(01).
                   88  WS-PRV-IS-PCP         VALUE 'Y'.
               10  WS-PRV-PCP-CAP-LIMIT      PIC 9(05).
               10  WS-PRV-PCP-CURR-PANEL     PIC 9(05).
           05  WS-PRV-PAYMENT-INFO.
               10  WS-PRV-PAY-TO-NAME       PIC X(60).
               10  WS-PRV-PAY-TO-ADDR       PIC X(80).
               10  WS-PRV-PAY-METHOD        PIC X(01).
                   88  WS-PRV-PAY-CHECK      VALUE 'C'.
                   88  WS-PRV-PAY-EFT        VALUE 'E'.
               10  WS-PRV-BANK-ROUT-NO      PIC X(09).
               10  WS-PRV-BANK-ACCT-NO      PIC X(17).
               10  WS-PRV-BANK-ACCT-TYPE    PIC X(01).
                   88  WS-PRV-ACCT-CHECKING  VALUE 'C'.
                   88  WS-PRV-ACCT-SAVINGS   VALUE 'S'.
               10  WS-PRV-1099-FLAG          PIC X(01).
                   88  WS-PRV-NEEDS-1099     VALUE 'Y'.
               10  WS-PRV-WITHHOLD-PCT       PIC S9(03)V99 COMP-3.
           05  WS-PRV-SANCTIONS.
               10  WS-PRV-OIG-EXCL-FLAG      PIC X(01).
                   88  WS-PRV-OIG-EXCLUDED   VALUE 'Y'.
               10  WS-PRV-OIG-CHECK-DT       PIC 9(08).
               10  WS-PRV-SAM-EXCL-FLAG      PIC X(01).
                   88  WS-PRV-SAM-EXCLUDED   VALUE 'Y'.
               10  WS-PRV-SAM-CHECK-DT       PIC 9(08).
               10  WS-PRV-SANCTION-FLAG      PIC X(01).
                   88  WS-PRV-SANCTIONED     VALUE 'Y'.
               10  WS-PRV-SANCTION-DT        PIC 9(08).
               10  WS-PRV-SANCTION-REASON    PIC X(04).
           05  WS-PRV-AUDIT-FIELDS.
               10  WS-PRV-CREATE-DT          PIC 9(08).
               10  WS-PRV-CREATE-USER        PIC X(08).
               10  WS-PRV-LAST-UPD-DT        PIC 9(08).
               10  WS-PRV-LAST-UPD-USER      PIC X(08).
               10  WS-PRV-RECORD-STATUS      PIC X(01).
                   88  WS-PRV-ACTIVE         VALUE 'A'.
                   88  WS-PRV-INACTIVE       VALUE 'I'.
                   88  WS-PRV-TERMINATED     VALUE 'T'.
