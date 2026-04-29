      *================================================================*
      * COPYBOOK: CPYSQLCA                                             *
      * DESCRIPTION: SYBASE SQL COMMUNICATION AREA AND DB INTERFACE    *
      * SYSTEM: HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)             *
      * AUTHOR: DATABASE ADMINISTRATION GROUP                          *
      * DATE WRITTEN: 1994-03-15                                       *
      *================================================================*

       01  WS-SQLCA.
           05  WS-SQLCAID              PIC X(08) VALUE 'SQLCA'.
           05  WS-SQLCABC              PIC S9(09) COMP VALUE +136.
           05  WS-SQLCODE              PIC S9(09) COMP VALUE +0.
           05  WS-SQLERRM.
               10  WS-SQLERRML        PIC S9(04) COMP VALUE +0.
               10  WS-SQLERRMC        PIC X(70) VALUE SPACES.
           05  WS-SQLERRP             PIC X(08) VALUE SPACES.
           05  WS-SQLERRD             PIC S9(09) COMP
                                      OCCURS 6 TIMES.
           05  WS-SQLWARN.
               10  WS-SQLWARN0        PIC X(01).
               10  WS-SQLWARN1        PIC X(01).
               10  WS-SQLWARN2        PIC X(01).
               10  WS-SQLWARN3        PIC X(01).
               10  WS-SQLWARN4        PIC X(01).
               10  WS-SQLWARN5        PIC X(01).
               10  WS-SQLWARN6        PIC X(01).
               10  WS-SQLWARN7        PIC X(01).
           05  WS-SQLEXT              PIC X(08) VALUE SPACES.

       01  WS-DB-CONTROL-AREA.
           05  WS-DB-RETURN-CODE      PIC S9(04) COMP VALUE +0.
               88  WS-DB-SUCCESS      VALUE +0.
               88  WS-DB-NOT-FOUND    VALUE +100.
               88  WS-DB-DUPLICATE    VALUE -803.
               88  WS-DB-DEADLOCK     VALUE -1205.
               88  WS-DB-TIMEOUT      VALUE -1224.
               88  WS-DB-CONSTRAINT   VALUE -545.
           05  WS-DB-ROW-COUNT        PIC S9(09) COMP VALUE +0.
           05  WS-DB-OPERATION        PIC X(10) VALUE SPACES.
           05  WS-DB-TABLE-NAME       PIC X(30) VALUE SPACES.
           05  WS-DB-ERROR-MSG        PIC X(256) VALUE SPACES.
           05  WS-DB-RETRY-COUNT      PIC 9(02) VALUE 0.
           05  WS-DB-MAX-RETRIES      PIC 9(02) VALUE 03.
           05  WS-DB-CONN-STATUS      PIC X(01) VALUE 'D'.
               88  WS-DB-CONNECTED    VALUE 'C'.
               88  WS-DB-DISCONNECTED VALUE 'D'.
           05  WS-DB-TXN-STATUS       PIC X(01) VALUE 'N'.
               88  WS-DB-TXN-ACTIVE   VALUE 'A'.
               88  WS-DB-TXN-NONE     VALUE 'N'.
           05  WS-DB-SERVER-NAME      PIC X(30) VALUE SPACES.
           05  WS-DB-DATABASE-NAME    PIC X(30) VALUE SPACES.
           05  WS-DB-USER-ID          PIC X(30) VALUE SPACES.

       01  WS-SYBASE-PARAMS.
           05  WS-SYB-SERVER          PIC X(30)
                                      VALUE 'HCPS_PROD_ASE'.
           05  WS-SYB-DATABASE        PIC X(30)
                                      VALUE 'HCPS_CLAIMS_DB'.
           05  WS-SYB-USER            PIC X(30)
                                      VALUE 'HCPS_BATCH'.
           05  WS-SYB-PASSWORD        PIC X(30)
                                      VALUE SPACES.
           05  WS-SYB-CHARSET         PIC X(20)
                                      VALUE 'iso_1'.
           05  WS-SYB-PACKET-SIZE     PIC 9(05)
                                      VALUE 4096.
           05  WS-SYB-TIMEOUT-SECS    PIC 9(05)
                                      VALUE 00300.
           05  WS-SYB-DEADLOCK-RETRY  PIC 9(02)
                                      VALUE 03.
