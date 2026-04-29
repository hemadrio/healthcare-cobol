# Healthcare Claims Processing System (HCPS)

A comprehensive COBOL-based healthcare claims processing system designed for
batch processing on mainframe (z/OS) environments, with GnuCOBOL support for
local development, testing, and education.

## System Architecture

```
                         HCPS System Architecture
  ============================================================

  +------------------+     +------------------+     +------------------+
  |  EDI Gateway     |     |  Enrollment      |     |  Provider        |
  |  (837I/837P)     |     |  Feed            |     |  Maintenance     |
  +--------+---------+     +--------+---------+     +--------+---------+
           |                        |                        |
           v                        v                        v
  +--------+---------+     +--------+---------+     +--------+---------+
  |  HCCLMVAL        |     |  HCELIGVR        |     |  HCPRVMNT        |
  |  Claims          |     |  Eligibility     |     |  Provider        |
  |  Validation      |     |  Verification    |     |  File Maint      |
  +--------+---------+     +--------+---------+     +------------------+
           |                        |
           v                        v
  +--------+------------------------+---------+
  |           DFSORT / SYNCSORT               |
  |        Sort by Payer/Plan/Provider        |
  +--------+----------------------------------+
           |
           v
  +--------+---------+     +------------------+
  |  HCCLMADJ        |---->|  VSAM Clusters   |
  |  Claims          |     |  - Fee Schedule  |
  |  Adjudication    |     |  - DRG Weights   |
  |  (Pricing,       |     |  - Accumulators  |
  |   Benefits,      |     |  - Auth Register |
  |   COB)           |     |  - Contracts     |
  +--------+---------+     +------------------+
           |
           +-------+-------+-------+
           |       |       |       |
           v       v       v       v
        +-----+ +-----+ +-----+ +-----+
        |PAID | |DENY | |PEND | |SUSP |
        +--+--+ +--+--+ +-----+ +-----+
           |       |
           v       v
  +--------+-------+---------+
  |  HCREMIT                 |
  |  Remittance / EOB        |
  |  - 835 Electronic        |
  |  - Paper Checks          |
  |  - EFT/ACH               |
  |  - Positive Pay           |
  |  - Member EOB            |
  +--------+-----------------+
           |
           v
  +--------+---------+     +------------------+
  |  HCRPTGEN        |     |  FTP/SFTP        |
  |  Report          |     |  Transmit 835    |
  |  Generation      |     |  to Clearinghouse|
  +------------------+     +------------------+

  Database: Sybase ASE         VSAM Clusters
  +--------------------+       +--------------------+
  | CLAIM_HEADER       |       | EDITRULES          |
  | CLAIM_DETAIL       |       | ICD10CODES         |
  | MEMBER_MASTER      |       | CPTCODES           |
  | PROVIDER_MASTER    |       | FEESCHEDULE        |
  | BENEFIT_PLAN       |       | DRGWEIGHTS         |
  | AUTHORIZATION      |       | BENACCUM           |
  | PAYMENT_HISTORY    |       | MEMBERMASTER       |
  | ACCUMULATOR        |       | PROVMASTER         |
  | 1099_TRACKING      |       | PROVCONTRACT       |
  +--------------------+       +--------------------+
```

## Batch Cycles

### Daily Batch (HCPS_DAILY.jcl)
Runs every business day at 01:00 AM EST. Processes incoming claims through
validation, eligibility verification, adjudication, and remittance generation.

| Step    | Program  | Description                         |
|---------|----------|-------------------------------------|
| STEP010 | HCCLMVAL | Claims validation and edit checks   |
| STEP020 | HCELIGVR | Eligibility verification batch      |
| STEP030 | SORT     | Sort validated claims by payer      |
| STEP040 | HCCLMADJ | Claims adjudication and pricing     |
| STEP050 | HCREMIT  | Remittance/EOB generation           |
| STEP060 | HCRPTGEN | Daily report generation             |
| STEP070 | BPXBATCH | FTP 835 files to clearinghouse      |
| STEP080 | IDCAMS   | GDG roll-forward and cleanup        |
| STEP090 | IDCAMS   | VSAM maintenance and verification   |

### Monthly Batch (HCPS_MONTHLY.jcl)
Runs on the 1st business day of each month at 02:00 AM EST.

| Step    | Program  | Description                         |
|---------|----------|-------------------------------------|
| STEP010 | HCPRVMNT | Provider maintenance (OIG/SAM)      |
| STEP020 | HCACCRCN | Benefit accumulator reconciliation  |
| STEP030 | HCMNTHCL | Monthly close processing            |
| STEP040 | HCCAPGEN | Capitation payment generation       |
| STEP050 | HCPREMRC | Premium reconciliation              |
| STEP060 | HC1099AC | 1099 accrual update                 |
| STEP070 | HCARCHIV | Data archive (aged claims)          |
| STEP080 | HCRPTGEN | Monthly management reports          |

### Year-End Batch (HCPS_YEAREND.jcl)
Runs January 1st at 04:00 AM EST. Requires manual release with CFO/CIO approval.

| Step    | Program   | Description                        |
|---------|-----------|------------------------------------|
| STEP010 | HCYRROLL  | Plan year rollover processing      |
| STEP020 | HCACCRST  | Accumulator reset (w/ carryover)   |
| STEP030 | HC1099GN  | 1099-NEC generation & reporting    |
| STEP040 | HCREGEXT  | Annual regulatory filing extract   |
| STEP050 | HCPURGE   | Archive and purge (7-year retain)  |
| STEP060 | HCFEELOAD | New year fee schedule load         |
| STEP070 | HCDRGUPD  | DRG weight table update            |
| STEP080 | HCCOLAUP  | COLA/inflation rate table update   |
| STEP090 | IDCAMS    | Year-end VSAM reorg and verify     |
| STEP100 | HCYRVRFY  | Year-end verification sign-off     |

## Prerequisites

- **GnuCOBOL 3.x+** (for local development and testing)
- **Bash 4.0+** (for test framework)
- **Sybase ASE** or **FreeTDS** (for database connectivity)
- On macOS: Homebrew
- On Linux: apt, yum, or dnf package manager

## Directory Structure

```
healthcare-cobol-system/
  COBOL/              COBOL source programs (.cbl)
  COPYBOOKS/          COBOL copybook definitions (.cpy)
  JCL/                JCL for batch execution
    HCPS_DAILY.jcl      Daily batch cycle
    HCPS_MONTHLY.jcl    Monthly batch cycle
    HCPS_YEAREND.jcl    Year-end batch cycle
  DATABASE/           Sybase DDL scripts
  SCRIPTS/            Shell scripts
    setup_gnucobol.sh   Environment setup
    compile_all.sh      Compilation script
    test_framework.sh   Test framework
  CONFIG/             Configuration files
    HCPS_CONFIG.dat     System configuration
  BUILD/              Compiled executables (generated)
  TEST/               Test data and output (generated)
  LOG/                Log files (generated)
```

## Setup Instructions

### 1. Clone the Repository

```bash
git clone <repository-url> healthcare-cobol-system
cd healthcare-cobol-system
```

### 2. Run Environment Setup

```bash
chmod +x SCRIPTS/*.sh
./SCRIPTS/setup_gnucobol.sh
```

This will:
- Detect your OS (macOS or Linux)
- Install GnuCOBOL via Homebrew or apt/yum
- Install FreeTDS (Sybase-compatible client)
- Create the directory structure
- Set up environment variables
- Run a compile/execute verification test

### 3. Source Environment

```bash
source ./hcps_env.sh
```

Add this to your shell profile for persistence:
```bash
echo "source $(pwd)/hcps_env.sh" >> ~/.bashrc
```

## How to Compile

### Compile All Programs

```bash
./SCRIPTS/compile_all.sh
```

### Compile a Single Program

```bash
./SCRIPTS/compile_all.sh --program=HCCLMVAL
```

### Compile with Debug Flags

```bash
./SCRIPTS/compile_all.sh --debug --listing
```

### Clean and Rebuild

```bash
./SCRIPTS/compile_all.sh --clean
```

## How to Run Tests

### Run All Tests

```bash
./SCRIPTS/test_framework.sh
```

### Run a Specific Test

```bash
./SCRIPTS/test_framework.sh --test=validation
./SCRIPTS/test_framework.sh --test=adjudication
./SCRIPTS/test_framework.sh --test=eligibility
./SCRIPTS/test_framework.sh --test=remittance
./SCRIPTS/test_framework.sh --test=provider
./SCRIPTS/test_framework.sh --test=reports
```

### Generate Test Data Only

```bash
./SCRIPTS/test_framework.sh --generate-only
```

### Run with Database Tests

```bash
export HCPS_DB_SERVER=localhost
export HCPS_DB_PORT=5000
export HCPS_DB_NAME=hcps_test
export HCPS_DB_USER=sa
export HCPS_DB_PASS=yourpassword
./SCRIPTS/test_framework.sh --with-db
```

### Test Scenarios

The test framework generates 15 claim scenarios:

| # | Scenario                      | Expected Result          |
|---|-------------------------------|--------------------------|
| 1 | Clean commercial claim        | PASS all edits           |
| 2 | Medicare with MSP             | PASS                     |
| 3 | Medicaid dual eligible        | PASS                     |
| 4 | Missing required fields       | REJECT - missing NPI     |
| 5 | Duplicate claim               | REJECT - duplicate       |
| 6 | Invalid NPI                   | REJECT - invalid NPI     |
| 7 | Invalid ICD-10 code           | REJECT - invalid dx      |
| 8 | Outside timely filing         | REJECT - timely filing   |
| 9 | Emergency / No Surprises Act  | PASS - special pricing   |
| 10| Opioid prescription           | PASS - monitoring flag   |
| 11| COVID vaccine                 | PASS - no cost sharing   |
| 12| Telehealth                    | PASS                     |
| 13| Preventive care (ACA)         | PASS - no cost sharing   |
| 14| High-dollar inpatient         | PASS - may pend          |
| 15| COB secondary claim           | PASS                     |

## Database Setup

### Using Sybase ASE

1. Install Sybase ASE or use SAP ASE Developer Edition
2. Create the database:
   ```sql
   CREATE DATABASE hcps_prod ON data_dev = '500M'
   ```
3. Run DDL scripts in order:
   ```bash
   isql -S HCPSPROD -U sa -P password -D hcps_prod \
     -i DATABASE/01_create_tables.sql
   isql -S HCPSPROD -U sa -P password -D hcps_prod \
     -i DATABASE/02_create_indexes.sql
   isql -S HCPSPROD -U sa -P password -D hcps_prod \
     -i DATABASE/03_create_procedures.sql
   ```

### Using FreeTDS (Alternative)

Configure `/usr/local/etc/freetds.conf` (or `CONFIG/freetds.conf`):
```ini
[HCPSPROD]
    host = hcpsdb01.corp.example.com
    port = 5000
    tds version = 5.0
    client charset = UTF-8
```

Test connectivity:
```bash
tsql -S HCPSPROD -U hcps_batch -P password
```

## Executing Batch Cycles

### On z/OS Mainframe

Submit JCL through JES2/JES3:
```
SUBMIT 'HCPS.PROD.JCLLIB(HCPSDALY)'
```

### Local Simulation with GnuCOBOL

The test framework simulates the batch cycle by setting DD-name environment
variables and executing each compiled program in sequence:

```bash
# Set input file paths via environment variables
export DD_CLMINPUT=./TEST/data/claims_input.dat
export DD_CLMVALID=./TEST/output/claims_validated.dat
export DD_CLMREJCT=./TEST/output/claims_rejected.dat

# Execute
./BUILD/HCCLMVAL
```

## Configuration

Edit `CONFIG/HCPS_CONFIG.dat` to modify:

- **Database connection** parameters (server, port, credentials)
- **Timely filing** limits by payer type
- **Payment thresholds** (auto-adjudication, high-dollar, pend)
- **Regulatory parameters** (NSA, telehealth, opioid monitoring)
- **ACA limits** (OOP max, HSA/FSA limits) -- updated annually
- **Pricing parameters** (conversion factor, DRG base rate)
- **Report parameters** (aging buckets, distribution lists)
- **Batch control** (schedule, checkpoints, notifications)
- **Clearinghouse** partner connection details

## Troubleshooting

### GnuCOBOL not found
```bash
# macOS
brew install gnucobol

# Ubuntu/Debian
sudo apt-get install gnucobol

# RHEL/CentOS
sudo yum install gnucobol
```

### Compilation errors with COPY statements
Ensure the COPYBOOKS directory is on the include path:
```bash
cobc -x -I ./COPYBOOKS -o BUILD/PROGRAM COBOL/PROGRAM.cbl
```

### EXEC SQL not supported
GnuCOBOL does not natively support embedded SQL. The compile script
automatically handles this by either:
1. Using the Sybase precompiler (`cpre`) if available
2. Commenting out EXEC SQL blocks and inserting mock stubs

### Runtime file not found errors
Ensure environment variables map DD names to file paths:
```bash
export DD_CLMINPUT=/path/to/input/file.dat
```

### FreeTDS connection issues
Test with:
```bash
tsql -S HCPSPROD -U username -P password
```
Check `CONFIG/freetds.conf` for correct host/port/TDS version.

### Listing files for debugging
Generate listing files during compilation:
```bash
./SCRIPTS/compile_all.sh --listing --debug
# Listings saved to BUILD/listings/
```

## File Inventory

| File                             | Description                                |
|----------------------------------|--------------------------------------------|
| `JCL/HCPS_DAILY.jcl`            | Daily batch cycle JCL                      |
| `JCL/HCPS_MONTHLY.jcl`          | Monthly batch cycle JCL                    |
| `JCL/HCPS_YEAREND.jcl`          | Year-end batch cycle JCL                   |
| `SCRIPTS/test_framework.sh`     | Comprehensive test framework               |
| `SCRIPTS/setup_gnucobol.sh`     | Environment setup script                   |
| `SCRIPTS/compile_all.sh`        | COBOL compilation script                   |
| `CONFIG/HCPS_CONFIG.dat`        | System configuration parameters            |
| `COBOL/HCCLMVAL.cbl`           | Claims validation program                  |
| `COBOL/HCELIGVR.cbl`           | Eligibility verification program           |
| `COBOL/HCCLMADJ.cbl`           | Claims adjudication program                |
| `COBOL/HCREMIT.cbl`            | Remittance/EOB generation program          |
| `COBOL/HCRPTGEN.cbl`           | Report generation program                  |
| `COBOL/HCPRVMNT.cbl`           | Provider maintenance program               |
| `COBOL/HCACCRCN.cbl`           | Accumulator reconciliation program         |
| `COBOL/HCMNTHCL.cbl`           | Monthly close program                      |
| `COBOL/HCCAPGEN.cbl`           | Capitation generation program              |
| `COBOL/HCPREMRC.cbl`           | Premium reconciliation program             |
| `COBOL/HC1099AC.cbl`           | 1099 accrual update program                |
| `COBOL/HC1099GN.cbl`           | 1099-NEC generation program                |
| `COBOL/HCYRROLL.cbl`           | Plan year rollover program                 |
| `COBOL/HCACCRST.cbl`           | Accumulator reset program                  |
| `COBOL/HCREGEXT.cbl`           | Regulatory filing extract program          |
| `COBOL/HCFEELOAD.cbl`          | Fee schedule load program                  |
| `COBOL/HCDRGUPD.cbl`           | DRG weight update program                  |
| `COBOL/HCCOLAUP.cbl`           | COLA/inflation update program              |
| `COBOL/HCYRVRFY.cbl`           | Year-end verification program              |
| `COBOL/HCARCHIV.cbl`           | Data archive program                       |
| `COBOL/HCPURGE.cbl`            | Data purge program                         |
| `COPYBOOKS/HCCLMREC.cpy`       | Claim record layout                        |
| `COPYBOOKS/HCMBRREC.cpy`       | Member record layout                       |
| `COPYBOOKS/HCPRVREC.cpy`       | Provider record layout                     |
| `COPYBOOKS/HCBENREC.cpy`       | Benefit plan record layout                 |
| `COPYBOOKS/HCACCREC.cpy`       | Accumulator record layout                  |
| `COPYBOOKS/HCPAYREC.cpy`       | Payment record layout                      |
| `COPYBOOKS/HC835REC.cpy`       | 835 EDI record layout                      |
| `DATABASE/01_create_tables.sql` | Table creation DDL                         |
| `DATABASE/02_create_indexes.sql`| Index creation DDL                         |
| `DATABASE/03_create_procedures.sql` | Stored procedure DDL                   |
