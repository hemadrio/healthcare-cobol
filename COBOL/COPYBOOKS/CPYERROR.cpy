      *================================================================*
      * COPYBOOK: CPYERROR                                             *
      * DESCRIPTION: ERROR HANDLING AND LOGGING STRUCTURES             *
      * SYSTEM: HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)             *
      * AUTHOR: SYSTEMS DEVELOPMENT GROUP                              *
      * DATE WRITTEN: 1994-03-15                                       *
      *================================================================*

       01  WS-ERROR-HANDLING-AREA.
           05  WS-ERR-SEVERITY            PIC X(01).
               88  WS-ERR-INFO            VALUE 'I'.
               88  WS-ERR-WARNING         VALUE 'W'.
               88  WS-ERR-ERROR           VALUE 'E'.
               88  WS-ERR-FATAL           VALUE 'F'.
           05  WS-ERR-CODE                PIC X(08).
           05  WS-ERR-PROGRAM             PIC X(08).
           05  WS-ERR-PARAGRAPH           PIC X(30).
           05  WS-ERR-MESSAGE             PIC X(256).
           05  WS-ERR-TIMESTAMP           PIC X(26).
           05  WS-ERR-COUNT               PIC 9(06) VALUE 0.
           05  WS-ERR-MAX-ALLOWED         PIC 9(06) VALUE 999.
           05  WS-ERR-THRESHOLD-HIT       PIC X(01) VALUE 'N'.
               88  WS-ERR-OVER-THRESHOLD  VALUE 'Y'.

       01  WS-ABEND-CONTROL.
           05  WS-ABEND-CODE             PIC X(04).
           05  WS-ABEND-PROGRAM          PIC X(08).
           05  WS-ABEND-REASON           PIC X(256).
           05  WS-ABEND-STEP             PIC X(08).
           05  WS-ABEND-TIMESTAMP        PIC X(26).
           05  WS-ABEND-DUMP-FLAG        PIC X(01) VALUE 'Y'.
               88  WS-ABEND-DUMP-YES     VALUE 'Y'.
               88  WS-ABEND-DUMP-NO      VALUE 'N'.

       01  WS-PROCESSING-STATS.
           05  WS-STAT-RECORDS-READ       PIC 9(09) VALUE 0.
           05  WS-STAT-RECORDS-WRITTEN    PIC 9(09) VALUE 0.
           05  WS-STAT-RECORDS-REJECTED   PIC 9(09) VALUE 0.
           05  WS-STAT-RECORDS-SKIPPED    PIC 9(09) VALUE 0.
           05  WS-STAT-RECORDS-UPDATED    PIC 9(09) VALUE 0.
           05  WS-STAT-RECORDS-DELETED    PIC 9(09) VALUE 0.
           05  WS-STAT-CLAIMS-APPROVED    PIC 9(09) VALUE 0.
           05  WS-STAT-CLAIMS-DENIED      PIC 9(09) VALUE 0.
           05  WS-STAT-CLAIMS-PENDED      PIC 9(09) VALUE 0.
           05  WS-STAT-TOTAL-CHARGED      PIC S9(13)V99 COMP-3
                                          VALUE +0.
           05  WS-STAT-TOTAL-ALLOWED      PIC S9(13)V99 COMP-3
                                          VALUE +0.
           05  WS-STAT-TOTAL-PAID         PIC S9(13)V99 COMP-3
                                          VALUE +0.
           05  WS-STAT-TOTAL-DENIED       PIC S9(13)V99 COMP-3
                                          VALUE +0.
           05  WS-STAT-START-TIME         PIC X(26).
           05  WS-STAT-END-TIME           PIC X(26).
           05  WS-STAT-ELAPSED-SECS       PIC 9(07) VALUE 0.
