#!/usr/bin/env bash
#==============================================================================
#  Healthcare Claims Processing System (HCPS)
#  COBOL Compilation Script
#
#  Compiles all COBOL programs in the correct dependency order using
#  GnuCOBOL (cobc). Handles COPY path resolution, listing generation,
#  and Sybase EXEC SQL precompilation (or mock).
#
#  Usage: ./compile_all.sh [OPTIONS]
#    --clean        Remove all build artifacts before compiling
#    --program=PGM  Compile only the specified program
#    --debug        Compile with debug flags (-g -ftraceall)
#    --listing      Generate listing files (.lst)
#    --verbose      Show detailed compilation output
#    --dry-run      Show commands without executing
#    --help         Show this help message
#
#  Change Log:
#    2024-01-20 R.Martinez  Initial creation
#    2024-09-01 S.Chen      Added precompiler step
#    2025-01-15 A.Patel     Added debug/listing options
#==============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

COBOL_SRC="${PROJECT_DIR}/COBOL"
COPYBOOK_DIR="${PROJECT_DIR}/COPYBOOKS"
BUILD_DIR="${PROJECT_DIR}/BUILD"
LISTING_DIR="${PROJECT_DIR}/BUILD/listings"
LOG_FILE="${PROJECT_DIR}/BUILD/compile.log"

# Compiler settings
COBC="cobc"
COBC_FLAGS="-x -Wall -O2"
COBC_COPY="-I ${COPYBOOK_DIR}"
COBC_DEBUG_FLAGS="-g -ftraceall -debug"

# Options
CLEAN=false
SPECIFIC_PROGRAM=""
DEBUG_MODE=false
GENERATE_LISTING=false
VERBOSE=false
DRY_RUN=false

# Counters
COMPILED=0
FAILED=0
SKIPPED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info()  { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass()  { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail()  { echo -e "${RED}[FAIL]${NC} $1"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC} $1"; }

#------------------------------------------------------------------------------
# Parse Arguments
#------------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)        CLEAN=true; shift ;;
        --program=*)    SPECIFIC_PROGRAM="${1#*=}"; shift ;;
        --debug)        DEBUG_MODE=true; shift ;;
        --listing)      GENERATE_LISTING=true; shift ;;
        --verbose)      VERBOSE=true; shift ;;
        --dry-run)      DRY_RUN=true; shift ;;
        --help)
            echo "Usage: $0 [--clean] [--program=PGM] [--debug] [--listing] [--verbose] [--dry-run]"
            exit 0
            ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

#------------------------------------------------------------------------------
# Verify Prerequisites
#------------------------------------------------------------------------------
verify_cobc() {
    if ! command -v "$COBC" &> /dev/null; then
        log_fail "GnuCOBOL compiler (cobc) not found on PATH"
        log_info "Run ./setup_gnucobol.sh to install"
        exit 1
    fi
    log_info "Using: $($COBC --version 2>&1 | head -1)"
}

#------------------------------------------------------------------------------
# Setup Build Directories
#------------------------------------------------------------------------------
setup_build() {
    if [ "$CLEAN" = true ]; then
        log_info "Cleaning build directory..."
        rm -rf "${BUILD_DIR:?}"/*
    fi

    mkdir -p "$BUILD_DIR"
    mkdir -p "$LISTING_DIR"

    # Initialize log
    echo "HCPS Compilation Log - $(date '+%Y-%m-%d %H:%M:%S')" > "$LOG_FILE"
    echo "Compiler: $($COBC --version 2>&1 | head -1)" >> "$LOG_FILE"
    echo "========================================" >> "$LOG_FILE"
}

#------------------------------------------------------------------------------
# Sybase EXEC SQL Precompiler Step
#------------------------------------------------------------------------------
# GnuCOBOL does not natively support Sybase EXEC SQL.
# This function either:
#   1. Invokes the Sybase precompiler (cpre) if available, or
#   2. Comments out EXEC SQL blocks and inserts mock calls
#
precompile_sql() {
    local src_file="$1"
    local out_file="$2"

    if command -v cpre &> /dev/null; then
        # Use actual Sybase precompiler
        log_info "  Precompiling EXEC SQL with cpre: $(basename "$src_file")"
        cpre -C COBOL -y "$src_file" -o "$out_file" >> "$LOG_FILE" 2>&1
        return $?
    fi

    # Mock precompiler: replace EXEC SQL blocks with DISPLAY stubs
    log_info "  Mock precompiling EXEC SQL: $(basename "$src_file")"

    sed -e 's/EXEC SQL/\*>EXEC SQL/g' \
        -e 's/END-EXEC/\*>END-EXEC/g' \
        "$src_file" > "$out_file"

    return 0
}

#------------------------------------------------------------------------------
# Compile a Single Program
#------------------------------------------------------------------------------
compile_program() {
    local pgm_name="$1"
    local src_file="${COBOL_SRC}/${pgm_name}.cbl"
    local out_file="${BUILD_DIR}/${pgm_name}"
    local lst_file="${LISTING_DIR}/${pgm_name}.lst"

    if [ ! -f "$src_file" ]; then
        log_warn "Source not found: $src_file (skipping)"
        SKIPPED=$((SKIPPED + 1))
        return
    fi

    # Check if source contains EXEC SQL
    local compile_src="$src_file"
    if grep -q "EXEC SQL" "$src_file" 2>/dev/null; then
        local precomp_file="${BUILD_DIR}/${pgm_name}_precomp.cbl"
        if precompile_sql "$src_file" "$precomp_file"; then
            compile_src="$precomp_file"
        else
            log_fail "Precompilation failed: $pgm_name"
            FAILED=$((FAILED + 1))
            return
        fi
    fi

    # Build compiler command
    local cmd="$COBC $COBC_FLAGS $COBC_COPY"

    if [ "$DEBUG_MODE" = true ]; then
        cmd="$cmd $COBC_DEBUG_FLAGS"
    fi

    if [ "$GENERATE_LISTING" = true ]; then
        cmd="$cmd -t $lst_file"
    fi

    cmd="$cmd -o $out_file $compile_src"

    if [ "$DRY_RUN" = true ]; then
        log_info "  [DRY-RUN] $cmd"
        return
    fi

    if [ "$VERBOSE" = true ]; then
        log_info "  CMD: $cmd"
    fi

    # Execute compilation
    echo "" >> "$LOG_FILE"
    echo "Compiling: $pgm_name" >> "$LOG_FILE"
    echo "Command: $cmd" >> "$LOG_FILE"

    local start_time
    start_time=$(date +%s)

    if eval "$cmd" >> "$LOG_FILE" 2>&1; then
        local end_time
        end_time=$(date +%s)
        local duration=$((end_time - start_time))
        COMPILED=$((COMPILED + 1))
        log_pass "Compiled: $pgm_name (${duration}s)"
        echo "Result: SUCCESS (${duration}s)" >> "$LOG_FILE"
    else
        local end_time
        end_time=$(date +%s)
        FAILED=$((FAILED + 1))
        log_fail "FAILED:   $pgm_name"
        echo "Result: FAILED" >> "$LOG_FILE"

        # Show last few lines of error
        if [ "$VERBOSE" = true ]; then
            tail -10 "$LOG_FILE"
        fi
    fi

    # Clean up precompiled source
    if [ -f "${BUILD_DIR}/${pgm_name}_precomp.cbl" ]; then
        rm -f "${BUILD_DIR}/${pgm_name}_precomp.cbl"
    fi
}

#------------------------------------------------------------------------------
# Program Compilation Order
# Programs are compiled in dependency order: utilities first,
# then core processing, then reporting.
#------------------------------------------------------------------------------
get_program_list() {
    # Tier 1: Utility and common modules (no dependencies)
    echo "HCCOMMON"    # Common routines and date handling

    # Tier 2: Core processing programs
    echo "HCCLMVAL"    # Claims validation
    echo "HCELIGVR"    # Eligibility verification
    echo "HCCLMADJ"    # Claims adjudication
    echo "HCREMIT"     # Remittance / EOB generation

    # Tier 3: Provider and maintenance programs
    echo "HCPRVMNT"    # Provider maintenance
    echo "HCACCRCN"    # Accumulator reconciliation
    echo "HCMNTHCL"    # Monthly close
    echo "HCCAPGEN"    # Capitation generation
    echo "HCPREMRC"    # Premium reconciliation

    # Tier 4: Tax and regulatory programs
    echo "HC1099AC"    # 1099 accrual
    echo "HC1099GN"    # 1099 generation
    echo "HCREGEXT"    # Regulatory extract

    # Tier 5: Year-end programs
    echo "HCYRROLL"    # Plan year rollover
    echo "HCACCRST"    # Accumulator reset
    echo "HCFEELOAD"   # Fee schedule load
    echo "HCDRGUPD"    # DRG weight update
    echo "HCCOLAUP"    # COLA/inflation update
    echo "HCYRVRFY"    # Year-end verification

    # Tier 6: Archive and housekeeping
    echo "HCARCHIV"    # Archive
    echo "HCPURGE"     # Purge

    # Tier 7: Reporting (depends on all others)
    echo "HCRPTGEN"    # Report generation
    echo "HCNOTIFY"    # Notification utility
}

#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
main() {
    echo "=================================================================="
    echo "  HCPS COBOL Compilation"
    echo "  $(date '+%Y-%m-%d %H:%M:%S')"
    echo "=================================================================="

    verify_cobc
    setup_build

    log_info "Source directory:   $COBOL_SRC"
    log_info "Copybook directory: $COPYBOOK_DIR"
    log_info "Build directory:    $BUILD_DIR"
    log_info "Debug mode:         $DEBUG_MODE"
    log_info "Generate listings:  $GENERATE_LISTING"

    if [ -n "$SPECIFIC_PROGRAM" ]; then
        log_info "Compiling single program: $SPECIFIC_PROGRAM"
        compile_program "$SPECIFIC_PROGRAM"
    else
        log_info "Compiling all programs in dependency order"
        echo ""

        while IFS= read -r pgm; do
            [ -z "$pgm" ] && continue
            compile_program "$pgm"
        done <<< "$(get_program_list)"
    fi

    echo ""
    echo "=================================================================="
    echo "  Compilation Summary"
    echo "  Compiled:  $COMPILED"
    echo "  Failed:    $FAILED"
    echo "  Skipped:   $SKIPPED"
    echo "  Log file:  $LOG_FILE"
    if [ "$GENERATE_LISTING" = true ]; then
        echo "  Listings:  $LISTING_DIR"
    fi
    echo "=================================================================="

    if [ "$FAILED" -gt 0 ]; then
        log_fail "Compilation completed with $FAILED error(s)"
        log_info "Check $LOG_FILE for details"
        exit 1
    elif [ "$COMPILED" -eq 0 ] && [ "$SKIPPED" -gt 0 ]; then
        log_warn "No programs compiled (all skipped - source files may be missing)"
        exit 0
    else
        log_pass "All programs compiled successfully"
        exit 0
    fi
}

main "$@"
