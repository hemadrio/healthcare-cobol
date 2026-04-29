      *================================================================*
      * COPYBOOK: CPYCLMLN                                             *
      * DESCRIPTION: CLAIM LINE/DETAIL RECORD LAYOUT                   *
      * SYSTEM: HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)             *
      * AUTHOR: SYSTEMS DEVELOPMENT GROUP                              *
      * DATE WRITTEN: 1994-03-15                                       *
      * DATE MODIFIED: 2015-10-01 - ICD-10 IMPLEMENTATION              *
      *================================================================*

       01  WS-CLAIM-LINE-REC.
           05  WS-CLN-KEY.
               10  WS-CLN-CLAIM-NO           PIC X(15).
               10  WS-CLN-SUFFIX             PIC X(02).
               10  WS-CLN-LINE-NO            PIC 9(03).
           05  WS-CLN-SERVICE-INFO.
               10  WS-CLN-REV-CD             PIC X(04).
               10  WS-CLN-HCPCS-CD           PIC X(05).
               10  WS-CLN-HCPCS-MOD-1        PIC X(02).
               10  WS-CLN-HCPCS-MOD-2        PIC X(02).
               10  WS-CLN-HCPCS-MOD-3        PIC X(02).
               10  WS-CLN-HCPCS-MOD-4        PIC X(02).
               10  WS-CLN-NDC-CD             PIC X(11).
               10  WS-CLN-NDC-QTY            PIC S9(07)V999 COMP-3.
               10  WS-CLN-NDC-UNIT-MEAS      PIC X(02).
               10  WS-CLN-PLACE-OF-SVC       PIC X(02).
                   88  WS-CLN-POS-OFFICE      VALUE '11'.
                   88  WS-CLN-POS-HOME        VALUE '12'.
                   88  WS-CLN-POS-INPATIENT   VALUE '21'.
                   88  WS-CLN-POS-OUTPATIENT  VALUE '22'.
                   88  WS-CLN-POS-ER          VALUE '23'.
                   88  WS-CLN-POS-ASC         VALUE '24'.
                   88  WS-CLN-POS-SNF         VALUE '31'.
                   88  WS-CLN-POS-LAB         VALUE '81'.
               10  WS-CLN-TYPE-OF-SVC        PIC X(01).
               10  WS-CLN-FROM-DOS           PIC 9(08).
               10  WS-CLN-THRU-DOS           PIC 9(08).
               10  WS-CLN-UNITS              PIC S9(05)V99 COMP-3.
               10  WS-CLN-UNIT-TYPE          PIC X(02).
                   88  WS-CLN-UNIT-EACH       VALUE 'UN'.
                   88  WS-CLN-UNIT-DAYS       VALUE 'DA'.
                   88  WS-CLN-UNIT-MINUTES    VALUE 'MN'.
                   88  WS-CLN-UNIT-VISITS     VALUE 'VS'.
                   88  WS-CLN-UNIT-MILES      VALUE 'ML'.
           05  WS-CLN-DIAG-POINTERS.
               10  WS-CLN-DIAG-PTR           PIC 9(02)
                                              OCCURS 4 TIMES.
           05  WS-CLN-PROVIDER-INFO.
               10  WS-CLN-REND-PROV-NPI      PIC X(10).
               10  WS-CLN-REND-PROV-NAME     PIC X(50).
               10  WS-CLN-REND-PROV-TAXNMY   PIC X(10).
               10  WS-CLN-REND-PROV-SPEC     PIC X(03).
               10  WS-CLN-PROV-PAR-STATUS    PIC X(01).
                   88  WS-CLN-PROV-PAR        VALUE 'P'.
                   88  WS-CLN-PROV-NONPAR     VALUE 'N'.
                   88  WS-CLN-PROV-OON        VALUE 'O'.
           05  WS-CLN-FINANCIAL-INFO.
               10  WS-CLN-CHARGE-AMT         PIC S9(07)V99 COMP-3.
               10  WS-CLN-ALLOWED-AMT        PIC S9(07)V99 COMP-3.
               10  WS-CLN-DEDUCTIBLE-AMT     PIC S9(07)V99 COMP-3.
               10  WS-CLN-COPAY-AMT          PIC S9(05)V99 COMP-3.
               10  WS-CLN-COINSURANCE-AMT    PIC S9(07)V99 COMP-3.
               10  WS-CLN-COB-AMT            PIC S9(07)V99 COMP-3.
               10  WS-CLN-WITHHOLD-AMT       PIC S9(05)V99 COMP-3.
               10  WS-CLN-NET-PAID-AMT       PIC S9(07)V99 COMP-3.
               10  WS-CLN-NON-COV-AMT        PIC S9(07)V99 COMP-3.
               10  WS-CLN-CONTRACTUAL-ADJ    PIC S9(07)V99 COMP-3.
               10  WS-CLN-PAT-RESP-AMT       PIC S9(07)V99 COMP-3.
           05  WS-CLN-PRICING-INFO.
               10  WS-CLN-FEE-SCHED-AMT      PIC S9(07)V99 COMP-3.
               10  WS-CLN-FEE-SCHED-CD       PIC X(06).
               10  WS-CLN-PRICE-METHOD        PIC X(02).
                   88  WS-CLN-PCT-OF-CHARGE   VALUE 'PC'.
                   88  WS-CLN-FEE-SCHEDULE    VALUE 'FS'.
                   88  WS-CLN-PER-DIEM        VALUE 'PD'.
                   88  WS-CLN-DRG-RATE        VALUE 'DR'.
                   88  WS-CLN-CASE-RATE       VALUE 'CR'.
                   88  WS-CLN-CAPITATED       VALUE 'CP'.
                   88  WS-CLN-MANUAL-PRICE    VALUE 'MP'.
               10  WS-CLN-CONTRACT-CD        PIC X(10).
               10  WS-CLN-CONTRACT-RATE      PIC S9(05)V9999 COMP-3.
           05  WS-CLN-STATUS-INFO.
               10  WS-CLN-LINE-STATUS        PIC X(02).
                   88  WS-CLN-APPROVED        VALUE 'AP'.
                   88  WS-CLN-DENIED          VALUE 'DN'.
                   88  WS-CLN-PENDED          VALUE 'PN'.
                   88  WS-CLN-ADJUSTED        VALUE 'AJ'.
               10  WS-CLN-DENY-REASON        PIC X(04).
               10  WS-CLN-DENY-REASON-2      PIC X(04).
               10  WS-CLN-PEND-REASON        PIC X(04).
               10  WS-CLN-RARC-CD            PIC X(05).
               10  WS-CLN-CARC-CD            PIC X(04).
               10  WS-CLN-CARC-GRP-CD        PIC X(02).
                   88  WS-CLN-CARC-CO         VALUE 'CO'.
                   88  WS-CLN-CARC-PR         VALUE 'PR'.
                   88  WS-CLN-CARC-PI         VALUE 'PI'.
                   88  WS-CLN-CARC-OA         VALUE 'OA'.
                   88  WS-CLN-CARC-CR         VALUE 'CR'.
           05  WS-CLN-AUTH-INFO.
               10  WS-CLN-AUTH-NO            PIC X(20).
               10  WS-CLN-AUTH-STATUS        PIC X(01).
                   88  WS-CLN-AUTH-APPROVED   VALUE 'A'.
                   88  WS-CLN-AUTH-DENIED     VALUE 'D'.
                   88  WS-CLN-AUTH-PENDING    VALUE 'P'.
                   88  WS-CLN-AUTH-NA         VALUE 'N'.
               10  WS-CLN-AUTH-QTY           PIC S9(05)V99 COMP-3.
               10  WS-CLN-AUTH-FROM-DT       PIC 9(08).
               10  WS-CLN-AUTH-THRU-DT       PIC 9(08).
           05  WS-CLN-AUDIT-INFO.
               10  WS-CLN-CREATE-DT          PIC 9(08).
               10  WS-CLN-CREATE-TM          PIC 9(06).
               10  WS-CLN-LAST-UPD-DT        PIC 9(08).
               10  WS-CLN-LAST-UPD-TM        PIC 9(06).
               10  WS-CLN-LAST-UPD-USER      PIC X(08).
