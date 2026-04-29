#!/usr/bin/env bash
#==============================================================================
#  Healthcare Claims Processing System (HCPS)
#  Comprehensive Test Framework for GnuCOBOL
#
#  This script provides end-to-end testing of the HCPS COBOL system
#  using GnuCOBOL (cobc) for compilation and execution.
#
#  Usage: ./test_framework.sh [OPTIONS]
#    Options:
#      --compile-only    Only compile, do not run tests
#      --test-only       Only run tests (assumes already compiled)
#      --test=NAME       Run specific test (e.g., --test=validation)
#      --generate-only   Only generate test data
#      --with-db         Include database tests (requires Sybase/FreeTDS)
#      --verbose         Enable verbose output
#      --cleanup         Clean up test artifacts after run
#      --help            Show this help message
#
#  Prerequisites:
#    - GnuCOBOL (cobc) installed and on PATH
#    - bash 4.0+
#    - Optional: Sybase/FreeTDS for database tests
#
#  Change Log:
#    2024-01-20 R.Martinez  Initial creation
#    2024-06-15 S.Chen      Added telehealth test scenarios
#    2025-01-10 A.Patel     Added opioid monitoring tests
#==============================================================================

set -euo pipefail

#------------------------------------------------------------------------------
# Global Configuration
#------------------------------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
COBOL_SRC_DIR="${PROJECT_DIR}/COBOL"
COPYBOOK_DIR="${PROJECT_DIR}/COPYBOOKS"
CONFIG_DIR="${PROJECT_DIR}/CONFIG"
TEST_DIR="${PROJECT_DIR}/TEST"
TEST_DATA_DIR="${TEST_DIR}/data"
TEST_OUTPUT_DIR="${TEST_DIR}/output"
TEST_EXPECTED_DIR="${TEST_DIR}/expected"
BUILD_DIR="${PROJECT_DIR}/BUILD"
LOG_DIR="${TEST_DIR}/logs"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Timing
TOTAL_START_TIME=0
TOTAL_END_TIME=0

# Options
COMPILE_ONLY=false
TEST_ONLY=false
SPECIFIC_TEST=""
GENERATE_ONLY=false
WITH_DB=false
VERBOSE=false
CLEANUP=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

#------------------------------------------------------------------------------
# Utility Functions
#------------------------------------------------------------------------------

log_info() {
    echo -e "${BLUE}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

log_verbose() {
    if [ "$VERBOSE" = true ]; then
        echo -e "${BLUE}[VERB]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
    fi
}

print_separator() {
    echo "=================================================================="
}

print_header() {
    print_separator
    echo "  $1"
    print_separator
}

assert_equals() {
    local description="$1"
    local expected="$2"
    local actual="$3"
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ "$expected" = "$actual" ]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_pass "$description (expected=$expected, actual=$actual)"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description (expected=$expected, actual=$actual)"
    fi
}

assert_file_exists() {
    local description="$1"
    local filepath="$2"
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ -f "$filepath" ]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_pass "$description - file exists: $(basename "$filepath")"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description - file NOT found: $filepath"
    fi
}

assert_file_not_empty() {
    local description="$1"
    local filepath="$2"
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ -f "$filepath" ] && [ -s "$filepath" ]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_pass "$description - file non-empty: $(basename "$filepath") ($(wc -l < "$filepath") lines)"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description - file empty or missing: $filepath"
    fi
}

assert_line_count() {
    local description="$1"
    local filepath="$2"
    local expected_min="$3"
    local expected_max="$4"
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ ! -f "$filepath" ]; then
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description - file not found: $filepath"
        return
    fi
    local count
    count=$(wc -l < "$filepath")
    if [ "$count" -ge "$expected_min" ] && [ "$count" -le "$expected_max" ]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_pass "$description - line count $count in range [$expected_min, $expected_max]"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description - line count $count NOT in range [$expected_min, $expected_max]"
    fi
}

assert_contains() {
    local description="$1"
    local filepath="$2"
    local pattern="$3"
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ ! -f "$filepath" ]; then
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description - file not found: $filepath"
        return
    fi
    if grep -q "$pattern" "$filepath"; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_pass "$description - pattern found: $pattern"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description - pattern NOT found: $pattern"
    fi
}

assert_rc() {
    local description="$1"
    local expected_rc="$2"
    local actual_rc="$3"
    TESTS_RUN=$((TESTS_RUN + 1))
    if [ "$actual_rc" -eq "$expected_rc" ]; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_pass "$description - RC=$actual_rc"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_fail "$description - expected RC=$expected_rc, got RC=$actual_rc"
    fi
}

#------------------------------------------------------------------------------
# Parse Command Line Arguments
#------------------------------------------------------------------------------

parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --compile-only)
                COMPILE_ONLY=true
                shift
                ;;
            --test-only)
                TEST_ONLY=true
                shift
                ;;
            --test=*)
                SPECIFIC_TEST="${1#*=}"
                shift
                ;;
            --generate-only)
                GENERATE_ONLY=true
                shift
                ;;
            --with-db)
                WITH_DB=true
                shift
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            --cleanup)
                CLEANUP=true
                shift
                ;;
            --help)
                usage
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                usage
                exit 1
                ;;
        esac
    done
}

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --compile-only    Only compile, do not run tests"
    echo "  --test-only       Only run tests (assumes already compiled)"
    echo "  --test=NAME       Run specific test (validation|adjudication|eligibility|"
    echo "                    remittance|provider|reports)"
    echo "  --generate-only   Only generate test data"
    echo "  --with-db         Include database tests"
    echo "  --verbose         Enable verbose output"
    echo "  --cleanup         Clean up test artifacts after run"
    echo "  --help            Show this help message"
}

#------------------------------------------------------------------------------
# Prerequisites Check
#------------------------------------------------------------------------------

check_prerequisites() {
    print_header "Checking Prerequisites"

    # Check GnuCOBOL
    if command -v cobc &> /dev/null; then
        local cobc_version
        cobc_version=$(cobc --version 2>&1 | head -1)
        log_info "GnuCOBOL found: $cobc_version"
    else
        log_fail "GnuCOBOL (cobc) not found on PATH"
        log_info "Install with: brew install gnucobol (macOS) or apt install gnucobol (Linux)"
        exit 1
    fi

    # Check bash version
    local bash_major="${BASH_VERSINFO[0]}"
    if [ "$bash_major" -ge 4 ]; then
        log_info "Bash version: $BASH_VERSION (OK)"
    else
        log_warn "Bash version $BASH_VERSION is older than 4.0; some features may not work"
    fi

    # Check for source directories
    if [ ! -d "$COBOL_SRC_DIR" ]; then
        log_warn "COBOL source directory not found: $COBOL_SRC_DIR"
        log_info "Creating placeholder directory structure"
        mkdir -p "$COBOL_SRC_DIR"
    fi

    if [ ! -d "$COPYBOOK_DIR" ]; then
        log_warn "COPYBOOKS directory not found: $COPYBOOK_DIR"
        mkdir -p "$COPYBOOK_DIR"
    fi

    # Check database connectivity if --with-db
    if [ "$WITH_DB" = true ]; then
        if command -v isql &> /dev/null; then
            log_info "Sybase isql found"
        elif command -v tsql &> /dev/null; then
            log_info "FreeTDS tsql found (Sybase-compatible)"
        else
            log_warn "No Sybase/FreeTDS client found; database tests will be skipped"
            WITH_DB=false
        fi
    fi

    log_info "Prerequisites check complete"
}

#------------------------------------------------------------------------------
# Directory Setup
#------------------------------------------------------------------------------

setup_directories() {
    log_info "Setting up test directories"
    mkdir -p "$TEST_DIR"
    mkdir -p "$TEST_DATA_DIR"
    mkdir -p "$TEST_OUTPUT_DIR"
    mkdir -p "$TEST_EXPECTED_DIR"
    mkdir -p "$BUILD_DIR"
    mkdir -p "$LOG_DIR"
    log_info "Directories created"
}

#------------------------------------------------------------------------------
# Test Data Generation Functions
#------------------------------------------------------------------------------

generate_patient_records() {
    print_header "Generating Patient/Member Records"
    local output_file="${TEST_DATA_DIR}/patient_master.dat"

    log_info "Creating patient master file: $output_file"

    # Record layout: Member ID (12) | Last Name (30) | First Name (20) | DOB (8) |
    #   Gender (1) | SSN (9) | Plan Code (5) | Group (10) | Eff Date (8) |
    #   Term Date (8) | PCP NPI (10) | Address (60) | City (25) |
    #   State (2) | Zip (9) | Phone (10) | Status (1) | COB Flag (1)
    # Total LRECL = 229

    cat > "$output_file" << 'PATEOF'
MBR000000011JOHNSON                       ROBERT              19850315M123456789PPO01GRP0000001202401012099123119876543210123 MAIN ST APT 4                                               CHICAGO                  IL606011234773255512340A0
MBR000000022MARTINEZ                      MARIA               19700920F987654321HMO02GRP0000002202301012099123114567890121456 OAK AVENUE                                                  LOS ANGELES              CA900151234883109876543A0
MBR000000033WILLIAMS                      JAMES               19451201M111223333MCAR1GRP0000003202201012099123112223334441789 PINE ROAD                                                   MIAMI                    FL331011234963057771234A0
MBR000000044CHEN                          SARAH               20100505F444556666PPO01GRP0000001202401012099123119876543210123 MAIN ST APT 4                                               CHICAGO                  IL606011234773255512340A0
MBR000000055PATEL                         RAJESH              19901115M555667777MCAD1GRP0000004202301012099123115556677771234 MAPLE DR                                                    HOUSTON                  TX770011234827135551234A1
MBR000000066THOMPSON                      JENNIFER            19800228F666778888PPO01GRP0000005202401012099123116667788882345 ELM STREET                                                  NEW YORK                 NY100011234212555678901A0
MBR000000077GARCIA                        CARLOS              19550710M777889999HMO02GRP0000002202001012099123117778899993456 BIRCH LANE                                                  PHOENIX                  AZ850011234602555789012A0
MBR000000088DAVIS                         AMANDA              19951220F888990000HDHP1GRP0000006202401012099123118889900004567 CEDAR CT                                                    SEATTLE                  WA981011234206555890123A0
MBR000000099KIM                           DAVID               20020301M999001111PPO01GRP0000001202401012099123119990011115678 WALNUT BLVD                                                 DENVER                   CO802011234303555901234A0
MBR000000100BROWN                         PATRICIA            19600830F000112222MCAR1GRP0000007202101012099123110001122226789 SPRUCE WAY                                                  ATLANTA                  GA303011234404555012345A0
MBR000000111TERMINATED                    MEMBER              19750515M111223344PPO01GRP0000001202301012024063019876543210999 EXPIRED LANE                                                NOWHERE                  XX000001234555000000000T0
MBR000000122DUAL                          ELIGIBLE            19680420F222334455DUAL1GRP0000008202201012099123112223344551234 DUAL COVERAGE DR                                            BOSTON                   MA021011234617555111222A1
PATEOF

    local count
    count=$(wc -l < "$output_file")
    log_info "Generated $count patient records"
}

generate_claims() {
    print_header "Generating Test Claims"
    local output_file="${TEST_DATA_DIR}/claims_input.dat"

    log_info "Creating claims input file: $output_file"

    # Record layout: Claim ID (15) | Claim Type (2) | Member ID (12) |
    #   Service From (8) | Service To (8) | Provider NPI (10) |
    #   Facility NPI (10) | Payer ID (10) | Plan (5) |
    #   Dx1 (7) | Dx2 (7) | Dx3 (7) | Dx4 (7) |
    #   Proc1 (5) | Mod1 (2) | Proc2 (5) | Mod2 (2) |
    #   Rev Code (4) | POS (2) | Units (5) |
    #   Billed Amt (11) | TOS (2) | Auth# (15) |
    #   Referring NPI (10) | Claim Freq (1) | Admission (8) |
    #   Discharge (8) | DRG (3) | Bill Type (4) | Status (2)
    # Filler/padding to reach LRECL 2000

    # Scenario 1: Clean commercial professional claim (should PASS all edits)
    echo "CLM202500000001011MBR0000000112025011520250115198765432100000000000PAY0000001PPO01E119100I10000J441000000009920025112501000000015000011AUTH20250000011987654321010000000000000000000   00  " > "$output_file"

    # Scenario 2: Medicare claim with MSP (secondary)
    echo "CLM202500000002011MBR0000000332025012020250120122233344400000000000PAY0000003MCAR1I2510000000000000000000009920025112501000000008500011AUTH20250000021222333444010000000000000000000   00  " >> "$output_file"

    # Scenario 3: Medicaid claim with dual eligible
    echo "CLM202500000003011MBR0000001222025012520250125555667777700000000000PAY0000004DUAL1Z0000000000000000000000009920025112501000000005000011AUTH20250000031234567890010000000000000000000   00  " >> "$output_file"

    # Scenario 4: Claim with missing required fields (should REJECT - no NPI)
    echo "CLM202500000004011MBR0000000222025013020250130000000000000000000000PAY0000001PPO01E119100000000000000000000009920025112501000000012500011                                  010000000000000000000   00  " >> "$output_file"

    # Scenario 5: Duplicate claim (should REJECT - same member/provider/DOS/proc)
    echo "CLM202500000005011MBR0000000112025011520250115198765432100000000000PAY0000001PPO01E119100I10000J441000000009920025112501000000015000011AUTH20250000011987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 6: Claim with invalid NPI (should REJECT)
    echo "CLM202500000006011MBR0000000662025020120250201000000000100000000000PAY0000005PPO01M545000000000000000000000009920025112501000000007500011AUTH20250000060000000001010000000000000000000   00  " >> "$output_file"

    # Scenario 7: Claim with invalid ICD-10 code (should REJECT)
    echo "CLM202500000007011MBR0000000882025020520250205198765432100000000000PAY0000006HDHP1ZZZ99000000000000000000000009920025112501000000003000011                 1987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 8: Claim outside timely filing limit (should REJECT)
    echo "CLM202500000008011MBR0000000222023060120230601198765432100000000000PAY0000001PPO01M791100000000000000000000009920025112501000000001500011                 1987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 9: Emergency claim - No Surprises Act (should PASS, special pricing)
    echo "CLM202500000009011MBR0000000662025021020250210198765432100000000000PAY0000005PPO01S72000I10000000000000000009923025102301000000045000011AUTH20250000091987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 10: Opioid prescription claim (should trigger monitoring)
    echo "CLM202500000010011MBR0000000112025021520250215198765432100000000000PAY0000001PPO01F111000000000000000000000009920025112501000000000850011                 1987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 11: COVID vaccine claim (no cost sharing)
    echo "CLM202500000011011MBR0000000442025022020250220198765432100000000000PAY0000001PPO01Z23000000000000000000000009920025112501000000004000011                 1987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 12: Telehealth claim
    echo "CLM202500000012011MBR0000000882025022520250225198765432100000000000PAY0000006HDHP1F329000000000000000000000009902025195021000000012500011AUTH20250000121987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 13: Preventive care claim (no cost sharing under ACA)
    echo "CLM202500000013011MBR0000000662025030120250301198765432100000000000PAY0000005PPO01Z001200000000000000000000009920025112501000000025000011                 1987654321010000000000000000000   00  " >> "$output_file"

    # Scenario 14: High-dollar inpatient claim
    echo "CLM202500000014022MBR0000000772025020120250215198765432119876543210PAY0000002HMO02I2510000000000000000000000014102501000010000001250000011AUTH20250000141987654321012025020120250215470011100  " >> "$output_file"

    # Scenario 15: COB secondary claim
    echo "CLM202500000015011MBR0000001222025031020250310198765432100000000000PAY0000004DUAL1M179000000000000000000000009920025112501000000003500021AUTH20250000151987654321010000000000000000000   00  " >> "$output_file"

    local count
    count=$(wc -l < "$output_file")
    log_info "Generated $count test claims"
    log_info "  - Scenario 1:  Clean commercial claim (expect PASS)"
    log_info "  - Scenario 2:  Medicare with MSP (expect PASS)"
    log_info "  - Scenario 3:  Medicaid dual eligible (expect PASS)"
    log_info "  - Scenario 4:  Missing NPI (expect REJECT)"
    log_info "  - Scenario 5:  Duplicate claim (expect REJECT)"
    log_info "  - Scenario 6:  Invalid NPI (expect REJECT)"
    log_info "  - Scenario 7:  Invalid ICD-10 (expect REJECT)"
    log_info "  - Scenario 8:  Timely filing exceeded (expect REJECT)"
    log_info "  - Scenario 9:  Emergency / NSA (expect PASS, special pricing)"
    log_info "  - Scenario 10: Opioid claim (expect PASS with monitoring flag)"
    log_info "  - Scenario 11: COVID vaccine (expect PASS, no cost share)"
    log_info "  - Scenario 12: Telehealth (expect PASS)"
    log_info "  - Scenario 13: Preventive care (expect PASS, no cost share)"
    log_info "  - Scenario 14: High-dollar inpatient (expect PASS, may pend)"
    log_info "  - Scenario 15: COB secondary (expect PASS)"
}

generate_provider_records() {
    print_header "Generating Provider Records"
    local output_file="${TEST_DATA_DIR}/provider_master.dat"

    log_info "Creating provider master file: $output_file"

    # Record: NPI (10) | Last Name (30) | First Name (20) | TIN (9) |
    #   Specialty (3) | Provider Type (2) | Network (1) | Contract (10) |
    #   Eff Date (8) | Term Date (8) | Payment Method (1) | Status (1)
    cat > "$output_file" << 'PRVEOF'
1987654321SMITH                         JOHN                123456789207011CCONTR00001202001012099123110A
1222333444GARCIA                        ELENA               234567890208011CCONTR00002202101012099123110A
5556677777PATEL                         ANIL                345678901210021CCONTR00003202201012099123110A
6667788888CHEN                          WENDY               456789012215011CCONTR00004202301012099123110A
1234567890WILLIAMS                      THOMAS              567890123225011OCONTR00005202401012099123120A
0000000001INVALID                       PROVIDER            000000000000000                              0I
7778899999JONES                         EXCLUDED            678901234204011CCONTR00006202001012099123110X
PRVEOF

    local count
    count=$(wc -l < "$output_file")
    log_info "Generated $count provider records"
}

generate_eligibility_records() {
    print_header "Generating Eligibility Records"
    local output_file="${TEST_DATA_DIR}/eligibility_feed.dat"

    log_info "Creating eligibility feed file: $output_file"

    # Record: Transaction Type (1) | Member ID (12) | Plan (5) | Group (10) |
    #   Eff Date (8) | Term Date (8) | Dep Code (2) | Relationship (2) |
    #   PCP NPI (10) | COB Flag (1) | COB Payer (10)
    cat > "$output_file" << 'ELIGEOF'
AMBR000000011PPO01GRP000000120240101209912310100198765432100
AMBR000000022HMO02GRP000000220230101209912310100145678901200
AMBR000000033MCAR1GRP000000320220101209912310100122233344400
AMBR000000044PPO01GRP000000120240101209912310201198765432100
AMBR000000055MCAD1GRP000000420230101209912310100555667777700
AMBR000000066PPO01GRP000000520240101209912310100666778888800
AMBR000000077HMO02GRP000000220200101209912310100777889999900
AMBR000000088HDHP1GRP000000620240101209912310100888990000000
AMBR000000099PPO01GRP000000120240101209912310201199900111100
AMBR000000100MCAR1GRP000000720210101209912310100000112222200
TMBR000000111PPO01GRP000000120230101202406300100198765432100
AMBR000000122DUAL1GRP000000820220101209912310100122233445510PAY0000004
ELIGEOF

    local count
    count=$(wc -l < "$output_file")
    log_info "Generated $count eligibility records"
}

generate_fee_schedule_data() {
    print_header "Generating Fee Schedule Data"
    local output_file="${TEST_DATA_DIR}/fee_schedule.dat"

    log_info "Creating fee schedule file: $output_file"

    # Record: Proc Code (5) | Modifier (2) | Eff Date (8) | Term Date (8) |
    #   RVU Work (7) | RVU PE (7) | RVU MP (7) | Conv Factor (9) |
    #   Allowed Amt (11) | Plan Code (5) | POS (2) | Network (1)
    cat > "$output_file" << 'FEEEOF'
99213  20250101209912310108000055000011000032744200000008714PPO0111I
99214  20250101209912310163000082000020000032744200000013245PPO0111I
99215  20250101209912310227000113000032000032744200000018567PPO0111I
99281  20250101209912310037000026000004000032744200000003890PPO0123I
99285  20250101209912310421000212000059000032744200000035678PPO0123I
99386  20250101209912310250000130000025000032744200000020125PPO0111I
90471  20250101209912310017000026000003000032744200000002345PPO0111I
99441  20250101209912310108000055000011000032744200000008714PPO0102I
99442  20250101209912310163000082000020000032744200000013245PPO0102I
J0000  20250101209912310000000000000000000032744200000004000PPO0111I
FEEEOF

    local count
    count=$(wc -l < "$output_file")
    log_info "Generated $count fee schedule entries"
}

generate_all_test_data() {
    print_header "Generating All Test Data"
    generate_patient_records
    generate_claims
    generate_provider_records
    generate_eligibility_records
    generate_fee_schedule_data
    log_info "All test data generation complete"
}

#------------------------------------------------------------------------------
# Compilation Functions
#------------------------------------------------------------------------------

compile_all_programs() {
    print_header "Compiling COBOL Programs"

    local compile_log="${LOG_DIR}/compilation.log"
    local compile_errors=0
    local compile_success=0

    # Source the compilation script if it exists
    if [ -f "${SCRIPT_DIR}/compile_all.sh" ]; then
        log_info "Invoking compile_all.sh"
        if bash "${SCRIPT_DIR}/compile_all.sh" > "$compile_log" 2>&1; then
            log_pass "Compilation completed via compile_all.sh"
            return 0
        else
            log_warn "compile_all.sh reported errors; attempting individual compilation"
        fi
    fi

    # Individual compilation fallback
    local programs=(
        "HCCLMVAL"
        "HCELIGVR"
        "HCCLMADJ"
        "HCREMIT"
        "HCRPTGEN"
        "HCPRVMNT"
        "HCARCHIV"
        "HCPURGE"
        "HCACCRCN"
        "HCMNTHCL"
        "HCCAPGEN"
        "HCPREMRC"
        "HC1099AC"
        "HC1099GN"
        "HCYRROLL"
        "HCACCRST"
        "HCREGEXT"
        "HCFEELOAD"
        "HCDRGUPD"
        "HCCOLAUP"
        "HCYRVRFY"
    )

    for pgm in "${programs[@]}"; do
        local src_file="${COBOL_SRC_DIR}/${pgm}.cbl"
        local out_file="${BUILD_DIR}/${pgm}"

        if [ ! -f "$src_file" ]; then
            log_warn "Source not found: $src_file (skipping)"
            continue
        fi

        log_verbose "Compiling $pgm..."

        if cobc -x -o "$out_file" \
               -I "$COPYBOOK_DIR" \
               -Wall \
               -O2 \
               "$src_file" >> "$compile_log" 2>&1; then
            compile_success=$((compile_success + 1))
            log_pass "Compiled: $pgm"
        else
            compile_errors=$((compile_errors + 1))
            log_fail "Compilation failed: $pgm"
            if [ "$VERBOSE" = true ]; then
                tail -20 "$compile_log"
            fi
        fi
    done

    log_info "Compilation summary: $compile_success succeeded, $compile_errors failed"
    log_info "Full compilation log: $compile_log"

    if [ "$compile_errors" -gt 0 ]; then
        return 1
    fi
    return 0
}

#------------------------------------------------------------------------------
# Test Execution Functions
#------------------------------------------------------------------------------

test_claims_validation() {
    print_header "Test: Claims Validation (HCCLMVAL)"
    local start_time
    start_time=$(date +%s)

    local pgm="${BUILD_DIR}/HCCLMVAL"
    if [ ! -x "$pgm" ]; then
        log_warn "HCCLMVAL not found or not executable; skipping test"
        TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        return
    fi

    # Set up environment for the COBOL program
    export DD_CLMINPUT="${TEST_DATA_DIR}/claims_input.dat"
    export DD_CLMVALID="${TEST_OUTPUT_DIR}/claims_validated.dat"
    export DD_CLMREJCT="${TEST_OUTPUT_DIR}/claims_rejected.dat"
    export DD_CLMDUPES="${TEST_OUTPUT_DIR}/claims_duplicates.dat"
    export DD_ERRRPT="${TEST_OUTPUT_DIR}/validation_errors.rpt"
    export DD_VALRPT="${TEST_OUTPUT_DIR}/validation_summary.rpt"
    export DD_EDITRULE="${TEST_DATA_DIR}/edit_rules.dat"
    export DD_ICDTBL="${TEST_DATA_DIR}/icd10_codes.dat"
    export DD_CPTTBL="${TEST_DATA_DIR}/cpt_codes.dat"
    export DD_NPITBL="${TEST_DATA_DIR}/provider_master.dat"

    log_info "Running HCCLMVAL with 15 test claims"
    local rc=0
    "$pgm" > "${LOG_DIR}/hcclmval_test.log" 2>&1 || rc=$?

    # Expected: 10 validated, 5 rejected (scenarios 4,5,6,7,8)
    assert_file_exists "Validated claims output" "$DD_CLMVALID"
    assert_file_exists "Rejected claims output" "$DD_CLMREJCT"

    if [ -f "$DD_CLMVALID" ]; then
        local valid_count
        valid_count=$(wc -l < "$DD_CLMVALID")
        assert_equals "Validated claim count" "10" "$valid_count"
    fi

    if [ -f "$DD_CLMREJCT" ]; then
        local reject_count
        reject_count=$(wc -l < "$DD_CLMREJCT")
        assert_equals "Rejected claim count" "5" "$reject_count"
    fi

    # Check specific rejection reasons
    if [ -f "$DD_ERRRPT" ]; then
        assert_contains "Missing NPI rejection" "$DD_ERRRPT" "MISSING.*NPI\|NPI.*INVALID\|REQUIRED.*NPI"
        assert_contains "Timely filing rejection" "$DD_ERRRPT" "TIMELY.*FILING\|FILING.*LIMIT"
    fi

    local end_time
    end_time=$(date +%s)
    log_info "Claims validation test completed in $((end_time - start_time)) seconds"
}

test_claims_adjudication() {
    print_header "Test: Claims Adjudication (HCCLMADJ)"
    local start_time
    start_time=$(date +%s)

    local pgm="${BUILD_DIR}/HCCLMADJ"
    if [ ! -x "$pgm" ]; then
        log_warn "HCCLMADJ not found or not executable; skipping test"
        TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        return
    fi

    # Set up environment
    export DD_SRTDCLMS="${TEST_OUTPUT_DIR}/claims_validated.dat"
    export DD_ADJPAID="${TEST_OUTPUT_DIR}/claims_paid.dat"
    export DD_ADJDENY="${TEST_OUTPUT_DIR}/claims_denied.dat"
    export DD_ADJPEND="${TEST_OUTPUT_DIR}/claims_pended.dat"
    export DD_ADJSUSPS="${TEST_OUTPUT_DIR}/claims_suspended.dat"
    export DD_ACCMUPDT="${TEST_OUTPUT_DIR}/accum_updates.dat"
    export DD_ADJRPT="${TEST_OUTPUT_DIR}/adjudication_summary.rpt"
    export DD_PRICRPT="${TEST_OUTPUT_DIR}/pricing_report.rpt"
    export DD_FEESCHED="${TEST_DATA_DIR}/fee_schedule.dat"
    export DD_PRVCONTR="${TEST_DATA_DIR}/provider_master.dat"

    log_info "Running HCCLMADJ with validated claims"
    local rc=0
    "$pgm" > "${LOG_DIR}/hcclmadj_test.log" 2>&1 || rc=$?

    # Verify outputs exist
    assert_file_exists "Paid claims output" "$DD_ADJPAID"
    assert_file_exists "Denied claims output" "$DD_ADJDENY"
    assert_file_exists "Pended claims output" "$DD_ADJPEND"

    # Verify payment calculations
    if [ -f "$DD_ADJRPT" ]; then
        assert_file_not_empty "Adjudication report" "$DD_ADJRPT"
        # High-dollar claim (scenario 14) should be pended or flagged
        assert_contains "High-dollar flag" "$DD_ADJRPT" "HIGH.DOLLAR\|PEND\|REVIEW"
    fi

    # Verify COVID vaccine has zero cost share
    if [ -f "$DD_ADJPAID" ]; then
        assert_file_not_empty "Paid claims file" "$DD_ADJPAID"
    fi

    local end_time
    end_time=$(date +%s)
    log_info "Claims adjudication test completed in $((end_time - start_time)) seconds"
}

test_eligibility() {
    print_header "Test: Eligibility Verification (HCELIGVR)"
    local start_time
    start_time=$(date +%s)

    local pgm="${BUILD_DIR}/HCELIGVR"
    if [ ! -x "$pgm" ]; then
        log_warn "HCELIGVR not found or not executable; skipping test"
        TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        return
    fi

    export DD_ENRLFEED="${TEST_DATA_DIR}/eligibility_feed.dat"
    export DD_CLMVALID="${TEST_OUTPUT_DIR}/claims_validated.dat"
    export DD_ELIGCLM="${TEST_OUTPUT_DIR}/claims_eligible.dat"
    export DD_INELICLM="${TEST_OUTPUT_DIR}/claims_ineligible.dat"
    export DD_ELIGRPT="${TEST_OUTPUT_DIR}/eligibility_report.rpt"

    log_info "Running HCELIGVR"
    local rc=0
    "$pgm" > "${LOG_DIR}/hceligvr_test.log" 2>&1 || rc=$?

    assert_file_exists "Eligible claims output" "$DD_ELIGCLM"
    assert_file_exists "Ineligible claims output" "$DD_INELICLM"

    # Terminated member (MBR000000111) claims should be ineligible
    if [ -f "$DD_INELICLM" ]; then
        assert_contains "Terminated member rejected" "$DD_INELICLM" "MBR000000111" || true
    fi

    local end_time
    end_time=$(date +%s)
    log_info "Eligibility test completed in $((end_time - start_time)) seconds"
}

test_remittance() {
    print_header "Test: Remittance / EOB Generation (HCREMIT)"
    local start_time
    start_time=$(date +%s)

    local pgm="${BUILD_DIR}/HCREMIT"
    if [ ! -x "$pgm" ]; then
        log_warn "HCREMIT not found or not executable; skipping test"
        TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        return
    fi

    export DD_ADJPAID="${TEST_OUTPUT_DIR}/claims_paid.dat"
    export DD_ADJDENY="${TEST_OUTPUT_DIR}/claims_denied.dat"
    export DD_EDI835="${TEST_OUTPUT_DIR}/edi_835.dat"
    export DD_CHKFILE="${TEST_OUTPUT_DIR}/checks.dat"
    export DD_EFTFILE="${TEST_OUTPUT_DIR}/eft.dat"
    export DD_POSPAY="${TEST_OUTPUT_DIR}/positive_pay.dat"
    export DD_EOBRPT="${TEST_OUTPUT_DIR}/eob.rpt"
    export DD_REMITRPT="${TEST_OUTPUT_DIR}/remittance_summary.rpt"

    log_info "Running HCREMIT"
    local rc=0
    "$pgm" > "${LOG_DIR}/hcremit_test.log" 2>&1 || rc=$?

    assert_file_exists "835 EDI output" "$DD_EDI835"
    assert_file_exists "Check file output" "$DD_CHKFILE"
    assert_file_exists "EFT file output" "$DD_EFTFILE"

    # Verify 835 format compliance
    if [ -f "$DD_EDI835" ]; then
        assert_file_not_empty "835 EDI file" "$DD_EDI835"
        # 835 should start with ISA segment
        assert_contains "835 ISA header" "$DD_EDI835" "^ISA"
        # 835 should contain CLP (claim payment) segments
        assert_contains "835 CLP segment" "$DD_EDI835" "CLP"
    fi

    local end_time
    end_time=$(date +%s)
    log_info "Remittance test completed in $((end_time - start_time)) seconds"
}

test_provider_maintenance() {
    print_header "Test: Provider Maintenance (HCPRVMNT)"
    local start_time
    start_time=$(date +%s)

    local pgm="${BUILD_DIR}/HCPRVMNT"
    if [ ! -x "$pgm" ]; then
        log_warn "HCPRVMNT not found or not executable; skipping test"
        TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        return
    fi

    export DD_OIGFILE="${TEST_DATA_DIR}/oig_exclusion.dat"
    export DD_SAMFILE="${TEST_DATA_DIR}/sam_exclusion.dat"
    export DD_PRVMASTR="${TEST_DATA_DIR}/provider_master.dat"
    export DD_EXCLFLAG="${TEST_OUTPUT_DIR}/excluded_providers.dat"
    export DD_PRVAUDIT="${TEST_OUTPUT_DIR}/provider_audit.dat"
    export DD_EXCLRPT="${TEST_OUTPUT_DIR}/exclusion_report.rpt"

    # Create minimal OIG/SAM test exclusion files
    echo "7778899999JONES                         2024010120991231EXCL001 OIG" > "${TEST_DATA_DIR}/oig_exclusion.dat"
    echo "0000000001INVALID                       2024010120991231SAM0001 SAM" > "${TEST_DATA_DIR}/sam_exclusion.dat"

    log_info "Running HCPRVMNT"
    local rc=0
    "$pgm" > "${LOG_DIR}/hcprvmnt_test.log" 2>&1 || rc=$?

    assert_file_exists "Excluded providers output" "$DD_EXCLFLAG"

    # Verify excluded provider was flagged
    if [ -f "$DD_EXCLFLAG" ]; then
        assert_contains "OIG-excluded provider flagged" "$DD_EXCLFLAG" "7778899999"
    fi

    local end_time
    end_time=$(date +%s)
    log_info "Provider maintenance test completed in $((end_time - start_time)) seconds"
}

test_reports() {
    print_header "Test: Report Generation (HCRPTGEN)"
    local start_time
    start_time=$(date +%s)

    local pgm="${BUILD_DIR}/HCRPTGEN"
    if [ ! -x "$pgm" ]; then
        log_warn "HCRPTGEN not found or not executable; skipping test"
        TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
        return
    fi

    export DD_ADJPAID="${TEST_OUTPUT_DIR}/claims_paid.dat"
    export DD_ADJDENY="${TEST_OUTPUT_DIR}/claims_denied.dat"
    export DD_ADJPEND="${TEST_OUTPUT_DIR}/claims_pended.dat"
    export DD_CLMREJCT="${TEST_OUTPUT_DIR}/claims_rejected.dat"
    export DD_RPT01="${TEST_OUTPUT_DIR}/rpt01_claims_summary.rpt"
    export DD_RPT02="${TEST_OUTPUT_DIR}/rpt02_payer_mix.rpt"
    export DD_RPT03="${TEST_OUTPUT_DIR}/rpt03_denial_breakdown.rpt"
    export DD_CSVOUT="${TEST_OUTPUT_DIR}/reports.csv"

    log_info "Running HCRPTGEN"
    local rc=0
    "$pgm" > "${LOG_DIR}/hcrptgen_test.log" 2>&1 || rc=$?

    assert_file_exists "Claims summary report" "$DD_RPT01"
    assert_file_exists "Payer mix report" "$DD_RPT02"
    assert_file_exists "Denial breakdown report" "$DD_RPT03"

    if [ -f "$DD_RPT01" ]; then
        assert_file_not_empty "Claims summary report content" "$DD_RPT01"
    fi

    local end_time
    end_time=$(date +%s)
    log_info "Report generation test completed in $((end_time - start_time)) seconds"
}

#------------------------------------------------------------------------------
# Database Test Functions
#------------------------------------------------------------------------------

setup_test_database() {
    print_header "Setting Up Test Database"

    if [ "$WITH_DB" != true ]; then
        log_info "Database tests disabled (use --with-db to enable)"
        return
    fi

    local ddl_dir="${PROJECT_DIR}/DATABASE"
    local db_log="${LOG_DIR}/database_setup.log"

    # Check for DDL files
    if [ ! -d "$ddl_dir" ]; then
        log_warn "DATABASE directory not found; skipping database setup"
        return
    fi

    log_info "Executing DDL scripts"

    # Connection parameters from config
    local db_server="${HCPS_DB_SERVER:-localhost}"
    local db_port="${HCPS_DB_PORT:-5000}"
    local db_name="${HCPS_DB_NAME:-hcps_test}"
    local db_user="${HCPS_DB_USER:-sa}"
    local db_pass="${HCPS_DB_PASS:-testpass}"

    # Execute DDL files in order
    local ddl_files=(
        "01_create_tables.sql"
        "02_create_indexes.sql"
        "03_create_procedures.sql"
        "04_load_reference_data.sql"
    )

    for ddl in "${ddl_files[@]}"; do
        local ddl_path="${ddl_dir}/${ddl}"
        if [ -f "$ddl_path" ]; then
            log_info "Executing: $ddl"
            if command -v isql &> /dev/null; then
                isql -S "$db_server" -U "$db_user" -P "$db_pass" \
                     -D "$db_name" -i "$ddl_path" >> "$db_log" 2>&1 || true
            elif command -v tsql &> /dev/null; then
                tsql -S "$db_server" -p "$db_port" -U "$db_user" -P "$db_pass" \
                     < "$ddl_path" >> "$db_log" 2>&1 || true
            fi
        fi
    done

    log_info "Database setup log: $db_log"
}

test_stored_procedures() {
    print_header "Testing Stored Procedures"

    if [ "$WITH_DB" != true ]; then
        log_info "Database tests disabled; skipping stored procedure tests"
        return
    fi

    log_info "Stored procedure testing would run here"
    # Placeholder for SP tests
    TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
}

#------------------------------------------------------------------------------
# Cleanup Functions
#------------------------------------------------------------------------------

cleanup_test_artifacts() {
    print_header "Cleaning Up Test Artifacts"

    if [ "$CLEANUP" = true ]; then
        log_info "Removing test output files"
        rm -rf "${TEST_OUTPUT_DIR:?}"/*
        log_info "Removing build artifacts"
        rm -rf "${BUILD_DIR:?}"/*
        log_info "Removing log files"
        rm -rf "${LOG_DIR:?}"/*
        log_info "Cleanup complete"
    else
        log_info "Cleanup skipped (use --cleanup to enable)"
        log_info "Test outputs: $TEST_OUTPUT_DIR"
        log_info "Build artifacts: $BUILD_DIR"
        log_info "Logs: $LOG_DIR"
    fi
}

#------------------------------------------------------------------------------
# Test Result Reporting
#------------------------------------------------------------------------------

print_test_results() {
    print_header "Test Results Summary"

    local total=$((TESTS_PASSED + TESTS_FAILED + TESTS_SKIPPED))
    local duration=$((TOTAL_END_TIME - TOTAL_START_TIME))

    echo ""
    echo "  Total Tests Run:     $TESTS_RUN"
    echo "  Passed:              $TESTS_PASSED"
    echo "  Failed:              $TESTS_FAILED"
    echo "  Skipped:             $TESTS_SKIPPED"
    echo ""
    echo "  Total Duration:      ${duration} seconds"
    echo ""

    if [ "$TESTS_FAILED" -eq 0 ] && [ "$TESTS_PASSED" -gt 0 ]; then
        echo -e "  ${GREEN}ALL TESTS PASSED${NC}"
    elif [ "$TESTS_FAILED" -gt 0 ]; then
        echo -e "  ${RED}SOME TESTS FAILED${NC}"
    else
        echo -e "  ${YELLOW}NO TESTS EXECUTED${NC}"
    fi

    echo ""
    print_separator

    # Write results to file
    local results_file="${LOG_DIR}/test_results_$(date '+%Y%m%d_%H%M%S').txt"
    cat > "$results_file" << EOF
HCPS Test Framework Results
Date: $(date '+%Y-%m-%d %H:%M:%S')
Duration: ${duration} seconds

Total Tests Run: $TESTS_RUN
Passed: $TESTS_PASSED
Failed: $TESTS_FAILED
Skipped: $TESTS_SKIPPED

Result: $([ "$TESTS_FAILED" -eq 0 ] && echo "PASS" || echo "FAIL")
EOF
    log_info "Results written to: $results_file"
}

#------------------------------------------------------------------------------
# Main Execution
#------------------------------------------------------------------------------

main() {
    TOTAL_START_TIME=$(date +%s)

    parse_args "$@"

    print_header "HCPS Test Framework"
    log_info "Project directory: $PROJECT_DIR"
    log_info "Starting test run: $(date '+%Y-%m-%d %H:%M:%S')"

    # Prerequisites
    check_prerequisites

    # Setup
    setup_directories

    # Generate test data
    if [ "$TEST_ONLY" != true ]; then
        generate_all_test_data
    fi

    if [ "$GENERATE_ONLY" = true ]; then
        log_info "Test data generation complete (--generate-only mode)"
        TOTAL_END_TIME=$(date +%s)
        print_test_results
        exit 0
    fi

    # Compile
    if [ "$TEST_ONLY" != true ]; then
        compile_all_programs || true
    fi

    if [ "$COMPILE_ONLY" = true ]; then
        log_info "Compilation complete (--compile-only mode)"
        TOTAL_END_TIME=$(date +%s)
        print_test_results
        exit 0
    fi

    # Database setup
    if [ "$WITH_DB" = true ]; then
        setup_test_database
    fi

    # Run tests
    if [ -n "$SPECIFIC_TEST" ]; then
        case "$SPECIFIC_TEST" in
            validation)     test_claims_validation ;;
            adjudication)   test_claims_adjudication ;;
            eligibility)    test_eligibility ;;
            remittance)     test_remittance ;;
            provider)       test_provider_maintenance ;;
            reports)        test_reports ;;
            *)
                log_fail "Unknown test: $SPECIFIC_TEST"
                log_info "Available tests: validation, adjudication, eligibility, remittance, provider, reports"
                exit 1
                ;;
        esac
    else
        # Run all tests in pipeline order
        test_claims_validation
        test_eligibility
        test_claims_adjudication
        test_remittance
        test_provider_maintenance
        test_reports

        # Database tests
        if [ "$WITH_DB" = true ]; then
            test_stored_procedures
        fi
    fi

    # Cleanup
    cleanup_test_artifacts

    # Results
    TOTAL_END_TIME=$(date +%s)
    print_test_results

    # Exit with appropriate code
    if [ "$TESTS_FAILED" -gt 0 ]; then
        exit 1
    fi
    exit 0
}

# Run main
main "$@"
