      *================================================================*
      * COPYBOOK: CPYCLMHD                                             *
      * DESCRIPTION: CLAIM HEADER RECORD LAYOUT - UB-04/CMS-1500       *
      * SYSTEM: HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)             *
      * AUTHOR: SYSTEMS DEVELOPMENT GROUP                              *
      * DATE WRITTEN: 1994-03-15                                       *
      * DATE MODIFIED: 2003-10-01 - HIPAA 837 COMPLIANCE               *
      *                2010-01-01 - ICD-10 READINESS                   *
      *                2015-10-01 - ICD-10 IMPLEMENTATION              *
      *================================================================*

       01  WS-CLAIM-HEADER-REC.
           05  WS-CLM-KEY.
               10  WS-CLM-NUMBER              PIC X(15).
               10  WS-CLM-SUFFIX              PIC X(02).
               10  WS-CLM-FACILITY-CD         PIC X(04).
           05  WS-CLM-TYPE-INFO.
               10  WS-CLM-TYPE                PIC X(02).
                   88  WS-CLM-INSTITUTIONAL   VALUE 'IN'.
                   88  WS-CLM-PROFESSIONAL    VALUE 'PR'.
                   88  WS-CLM-DENTAL          VALUE 'DN'.
                   88  WS-CLM-PHARMACY        VALUE 'PH'.
               10  WS-CLM-FORM-TYPE           PIC X(02).
                   88  WS-CLM-UB04            VALUE 'UB'.
                   88  WS-CLM-CMS1500         VALUE 'CM'.
                   88  WS-CLM-ADA             VALUE 'AD'.
               10  WS-CLM-FREQ-CD            PIC X(01).
                   88  WS-CLM-ORIGINAL        VALUE '1'.
                   88  WS-CLM-REPLACEMENT     VALUE '7'.
                   88  WS-CLM-VOID            VALUE '8'.
               10  WS-CLM-SOURCE-CD          PIC X(02).
                   88  WS-CLM-ELECTRONIC      VALUE 'EL'.
                   88  WS-CLM-PAPER           VALUE 'PP'.
                   88  WS-CLM-MANUAL-ENTRY    VALUE 'MN'.
                   88  WS-CLM-EDI-837I        VALUE '7I'.
                   88  WS-CLM-EDI-837P        VALUE '7P'.
           05  WS-CLM-STATUS-INFO.
               10  WS-CLM-STATUS             PIC X(02).
                   88  WS-CLM-RECEIVED        VALUE 'RC'.
                   88  WS-CLM-VALIDATED       VALUE 'VL'.
                   88  WS-CLM-IN-ADJUD        VALUE 'AJ'.
                   88  WS-CLM-PENDED          VALUE 'PN'.
                   88  WS-CLM-APPROVED        VALUE 'AP'.
                   88  WS-CLM-DENIED          VALUE 'DN'.
                   88  WS-CLM-PAID            VALUE 'PD'.
                   88  WS-CLM-APPEALED        VALUE 'AL'.
                   88  WS-CLM-VOIDED          VALUE 'VD'.
                   88  WS-CLM-SUSPENDED       VALUE 'SU'.
               10  WS-CLM-SUB-STATUS         PIC X(03).
               10  WS-CLM-PEND-REASON        PIC X(04).
               10  WS-CLM-DENY-REASON        PIC X(04).
               10  WS-CLM-DENY-REASON-2      PIC X(04).
               10  WS-CLM-DENY-REASON-3      PIC X(04).
           05  WS-CLM-PATIENT-INFO.
               10  WS-CLM-PAT-MRN            PIC X(12).
               10  WS-CLM-PAT-ACCT-NO        PIC X(20).
               10  WS-CLM-PAT-LAST-NAME      PIC X(35).
               10  WS-CLM-PAT-FIRST-NAME     PIC X(25).
               10  WS-CLM-PAT-DOB            PIC 9(08).
               10  WS-CLM-PAT-GENDER         PIC X(01).
               10  WS-CLM-PAT-RELATION       PIC X(02).
                   88  WS-CLM-PAT-SELF        VALUE '18'.
                   88  WS-CLM-PAT-SPOUSE      VALUE '01'.
                   88  WS-CLM-PAT-CHILD       VALUE '19'.
                   88  WS-CLM-PAT-OTHER-REL   VALUE '20'.
           05  WS-CLM-SUBSCRIBER-INFO.
               10  WS-CLM-SUB-MEMBER-ID      PIC X(20).
               10  WS-CLM-SUB-GROUP-NO       PIC X(15).
               10  WS-CLM-SUB-LAST-NAME      PIC X(35).
               10  WS-CLM-SUB-FIRST-NAME     PIC X(25).
               10  WS-CLM-SUB-DOB            PIC 9(08).
               10  WS-CLM-SUB-EMPLOYER       PIC X(40).
           05  WS-CLM-PAYER-INFO.
               10  WS-CLM-PAYER-CD           PIC X(08).
               10  WS-CLM-PAYER-NAME         PIC X(40).
               10  WS-CLM-PAYER-TYPE         PIC X(02).
                   88  WS-CLM-PAYER-COMM     VALUE 'CO'.
                   88  WS-CLM-PAYER-MCARE    VALUE 'MC'.
                   88  WS-CLM-PAYER-MCAID    VALUE 'MD'.
                   88  WS-CLM-PAYER-TRICARE  VALUE 'TR'.
                   88  WS-CLM-PAYER-VA       VALUE 'VA'.
                   88  WS-CLM-PAYER-WC       VALUE 'WC'.
                   88  WS-CLM-PAYER-SELFPAY  VALUE 'SP'.
               10  WS-CLM-COB-IND           PIC X(01).
                   88  WS-CLM-HAS-COB        VALUE 'Y'.
               10  WS-CLM-PAYER-SEQ         PIC 9(01).
                   88  WS-CLM-PRIMARY        VALUE 1.
                   88  WS-CLM-SECONDARY      VALUE 2.
                   88  WS-CLM-TERTIARY       VALUE 3.
           05  WS-CLM-PROVIDER-INFO.
               10  WS-CLM-BILL-PROV-NPI     PIC X(10).
               10  WS-CLM-BILL-PROV-TAX-ID  PIC X(09).
               10  WS-CLM-BILL-PROV-NAME    PIC X(50).
               10  WS-CLM-BILL-PROV-TAXNMY  PIC X(10).
               10  WS-CLM-REND-PROV-NPI     PIC X(10).
               10  WS-CLM-REND-PROV-NAME    PIC X(50).
               10  WS-CLM-REND-PROV-TAXNMY  PIC X(10).
               10  WS-CLM-REFER-PROV-NPI    PIC X(10).
               10  WS-CLM-REFER-PROV-NAME   PIC X(50).
               10  WS-CLM-ATTEND-PROV-NPI   PIC X(10).
               10  WS-CLM-ATTEND-PROV-NAME  PIC X(50).
               10  WS-CLM-OPER-PROV-NPI     PIC X(10).
               10  WS-CLM-FACILITY-NPI      PIC X(10).
               10  WS-CLM-FACILITY-NAME     PIC X(50).
           05  WS-CLM-DATE-INFO.
               10  WS-CLM-FROM-DOS          PIC 9(08).
               10  WS-CLM-THRU-DOS          PIC 9(08).
               10  WS-CLM-ADMIT-DT          PIC 9(08).
               10  WS-CLM-DISCH-DT          PIC 9(08).
               10  WS-CLM-RECEIVED-DT       PIC 9(08).
               10  WS-CLM-ADJUD-DT          PIC 9(08).
               10  WS-CLM-PAID-DT           PIC 9(08).
               10  WS-CLM-STMT-FROM-DT      PIC 9(08).
               10  WS-CLM-STMT-THRU-DT      PIC 9(08).
               10  WS-CLM-TIMELY-FILE-DT    PIC 9(08).
           05  WS-CLM-INSTITUTIONAL-INFO.
               10  WS-CLM-ADMIT-TYPE        PIC X(01).
                   88  WS-CLM-EMERGENCY      VALUE '1'.
                   88  WS-CLM-URGENT         VALUE '2'.
                   88  WS-CLM-ELECTIVE       VALUE '3'.
                   88  WS-CLM-NEWBORN        VALUE '4'.
               10  WS-CLM-ADMIT-SOURCE      PIC X(02).
               10  WS-CLM-DISCH-STATUS      PIC X(02).
               10  WS-CLM-BILL-TYPE         PIC X(04).
               10  WS-CLM-BILL-TYPE-R REDEFINES WS-CLM-BILL-TYPE.
                   15  WS-CLM-LEAD-ZERO     PIC X(01).
                   15  WS-CLM-TOB-FAC-TYPE  PIC X(01).
                   15  WS-CLM-TOB-FREQ-CD   PIC X(01).
                   15  WS-CLM-TOB-CLM-CLASS PIC X(01).
               10  WS-CLM-DRG-CD           PIC X(04).
               10  WS-CLM-DRG-WEIGHT       PIC S9(03)V9999 COMP-3.
               10  WS-CLM-LOS              PIC 9(04).
               10  WS-CLM-COVERED-DAYS     PIC 9(04).
               10  WS-CLM-NON-COV-DAYS     PIC 9(04).
               10  WS-CLM-CONDITION-CDS.
                   15  WS-CLM-COND-CD       PIC X(02)
                                            OCCURS 12 TIMES.
               10  WS-CLM-OCCURRENCE-CDS.
                   15  WS-CLM-OCC-CD        PIC X(02)
                                            OCCURS 8 TIMES.
                   15  WS-CLM-OCC-DT        PIC 9(08)
                                            OCCURS 8 TIMES.
               10  WS-CLM-VALUE-CDS.
                   15  WS-CLM-VAL-CD        PIC X(02)
                                            OCCURS 12 TIMES.
                   15  WS-CLM-VAL-AMT       PIC S9(07)V99 COMP-3
                                            OCCURS 12 TIMES.
           05  WS-CLM-DIAGNOSIS-INFO.
               10  WS-CLM-ICD-VERSION       PIC X(01).
                   88  WS-CLM-ICD9          VALUE '9'.
                   88  WS-CLM-ICD10         VALUE '0'.
               10  WS-CLM-PRINC-DIAG       PIC X(08).
               10  WS-CLM-ADMIT-DIAG       PIC X(08).
               10  WS-CLM-E-CODE           PIC X(08).
               10  WS-CLM-DIAG-TABLE.
                   15  WS-CLM-DIAG-CD       PIC X(08)
                                            OCCURS 25 TIMES.
                   15  WS-CLM-DIAG-POA      PIC X(01)
                                            OCCURS 25 TIMES.
               10  WS-CLM-PROC-TABLE.
                   15  WS-CLM-ICD-PROC-CD   PIC X(08)
                                            OCCURS 25 TIMES.
                   15  WS-CLM-PROC-DT       PIC 9(08)
                                            OCCURS 25 TIMES.
           05  WS-CLM-FINANCIAL-INFO.
               10  WS-CLM-TOTAL-CHARGES     PIC S9(09)V99 COMP-3.
               10  WS-CLM-NON-COV-CHARGES   PIC S9(09)V99 COMP-3.
               10  WS-CLM-ALLOWED-AMT       PIC S9(09)V99 COMP-3.
               10  WS-CLM-DEDUCTIBLE-AMT    PIC S9(07)V99 COMP-3.
               10  WS-CLM-COPAY-AMT         PIC S9(07)V99 COMP-3.
               10  WS-CLM-COINSURANCE-AMT   PIC S9(07)V99 COMP-3.
               10  WS-CLM-COB-AMT           PIC S9(09)V99 COMP-3.
               10  WS-CLM-WITHHOLD-AMT      PIC S9(07)V99 COMP-3.
               10  WS-CLM-INTEREST-AMT      PIC S9(07)V99 COMP-3.
               10  WS-CLM-PENALTY-AMT       PIC S9(07)V99 COMP-3.
               10  WS-CLM-NET-PAID-AMT      PIC S9(09)V99 COMP-3.
               10  WS-CLM-PAT-RESP-AMT      PIC S9(09)V99 COMP-3.
               10  WS-CLM-CONTRACTUAL-ADJ   PIC S9(09)V99 COMP-3.
               10  WS-CLM-CHECK-NO          PIC X(12).
               10  WS-CLM-CHECK-DT          PIC 9(08).
               10  WS-CLM-EFT-TRACE-NO     PIC X(20).
           05  WS-CLM-AUDIT-INFO.
               10  WS-CLM-CREATE-DT         PIC 9(08).
               10  WS-CLM-CREATE-TM         PIC 9(06).
               10  WS-CLM-CREATE-USER       PIC X(08).
               10  WS-CLM-LAST-UPD-DT       PIC 9(08).
               10  WS-CLM-LAST-UPD-TM       PIC 9(06).
               10  WS-CLM-LAST-UPD-USER     PIC X(08).
               10  WS-CLM-BATCH-NO          PIC X(10).
               10  WS-CLM-ORIG-CLM-NO       PIC X(15).
               10  WS-CLM-PRIOR-AUTH-NO     PIC X(20).
               10  WS-CLM-REF-NO            PIC X(20).
               10  WS-CLM-ICN              PIC X(20).
