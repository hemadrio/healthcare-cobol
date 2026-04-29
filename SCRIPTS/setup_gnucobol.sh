#!/usr/bin/env bash
#==============================================================================
#  Healthcare Claims Processing System (HCPS)
#  GnuCOBOL Environment Setup Script
#
#  This script detects the operating system, installs GnuCOBOL and
#  supporting libraries, configures environment variables, compiles
#  all COBOL programs, and verifies the installation.
#
#  Usage: ./setup_gnucobol.sh [--skip-install] [--skip-compile] [--verbose]
#
#  Change Log:
#    2024-01-20 R.Martinez  Initial creation
#    2024-09-01 S.Chen      Added FreeTDS support
#    2025-01-15 A.Patel     Added macOS ARM support
#==============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SKIP_INSTALL=false
SKIP_COMPILE=false
VERBOSE=false

#------------------------------------------------------------------------------
# Parse Arguments
#------------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-install) SKIP_INSTALL=true; shift ;;
        --skip-compile) SKIP_COMPILE=true; shift ;;
        --verbose)      VERBOSE=true; shift ;;
        --help)
            echo "Usage: $0 [--skip-install] [--skip-compile] [--verbose]"
            exit 0
            ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

log_info()  { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass()  { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail()  { echo -e "${RED}[FAIL]${NC} $1"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC} $1"; }

print_banner() {
    echo "=================================================================="
    echo "  HCPS - GnuCOBOL Environment Setup"
    echo "  $(date '+%Y-%m-%d %H:%M:%S')"
    echo "=================================================================="
}

#------------------------------------------------------------------------------
# Detect Operating System
#------------------------------------------------------------------------------
detect_os() {
    log_info "Detecting operating system..."

    OS_TYPE="unknown"
    OS_VERSION=""
    ARCH=$(uname -m)

    case "$(uname -s)" in
        Darwin*)
            OS_TYPE="macos"
            OS_VERSION=$(sw_vers -productVersion 2>/dev/null || echo "unknown")
            log_info "Detected: macOS $OS_VERSION ($ARCH)"
            ;;
        Linux*)
            OS_TYPE="linux"
            if [ -f /etc/os-release ]; then
                . /etc/os-release
                OS_VERSION="$ID $VERSION_ID"
            fi
            log_info "Detected: Linux $OS_VERSION ($ARCH)"
            ;;
        *)
            log_fail "Unsupported operating system: $(uname -s)"
            exit 1
            ;;
    esac
}

#------------------------------------------------------------------------------
# Install GnuCOBOL
#------------------------------------------------------------------------------
install_gnucobol() {
    if [ "$SKIP_INSTALL" = true ]; then
        log_info "Skipping installation (--skip-install)"
        return
    fi

    log_info "Installing GnuCOBOL..."

    case "$OS_TYPE" in
        macos)
            # Check for Homebrew
            if ! command -v brew &> /dev/null; then
                log_info "Homebrew not found. Installing Homebrew..."
                /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
            fi

            log_info "Installing GnuCOBOL via Homebrew..."
            brew install gnucobol 2>/dev/null || brew upgrade gnucobol 2>/dev/null || true

            # Install additional dependencies
            log_info "Installing additional dependencies..."
            brew install gmp berkeley-db json-c 2>/dev/null || true
            ;;
        linux)
            if command -v apt-get &> /dev/null; then
                # Debian/Ubuntu
                log_info "Installing GnuCOBOL via apt..."
                sudo apt-get update -qq
                sudo apt-get install -y gnucobol libcob4-dev \
                    build-essential libgmp-dev libdb-dev
            elif command -v yum &> /dev/null; then
                # RHEL/CentOS
                log_info "Installing GnuCOBOL via yum..."
                sudo yum install -y gnucobol gnucobol-devel \
                    gcc gmp-devel libdb-devel
            elif command -v dnf &> /dev/null; then
                # Fedora
                log_info "Installing GnuCOBOL via dnf..."
                sudo dnf install -y gnucobol gnucobol-devel \
                    gcc gmp-devel libdb-devel
            else
                log_fail "No supported package manager found (apt/yum/dnf)"
                exit 1
            fi
            ;;
    esac

    # Verify installation
    if command -v cobc &> /dev/null; then
        log_pass "GnuCOBOL installed: $(cobc --version 2>&1 | head -1)"
    else
        log_fail "GnuCOBOL installation failed"
        exit 1
    fi
}

#------------------------------------------------------------------------------
# Install Sybase Client Libraries (FreeTDS)
#------------------------------------------------------------------------------
install_sybase_client() {
    if [ "$SKIP_INSTALL" = true ]; then
        log_info "Skipping Sybase client installation (--skip-install)"
        return
    fi

    log_info "Installing Sybase-compatible client libraries (FreeTDS)..."

    case "$OS_TYPE" in
        macos)
            brew install freetds 2>/dev/null || brew upgrade freetds 2>/dev/null || true
            ;;
        linux)
            if command -v apt-get &> /dev/null; then
                sudo apt-get install -y freetds-dev freetds-bin \
                    unixodbc unixodbc-dev tdsodbc
            elif command -v yum &> /dev/null; then
                sudo yum install -y freetds freetds-devel \
                    unixODBC unixODBC-devel
            elif command -v dnf &> /dev/null; then
                sudo dnf install -y freetds freetds-devel \
                    unixODBC unixODBC-devel
            fi
            ;;
    esac

    # Verify FreeTDS
    if command -v tsql &> /dev/null; then
        log_pass "FreeTDS installed: $(tsql -C 2>&1 | head -1 || echo 'version check N/A')"
    else
        log_warn "FreeTDS not found; database features will be limited"
    fi
}

#------------------------------------------------------------------------------
# Set Environment Variables
#------------------------------------------------------------------------------
setup_environment() {
    log_info "Configuring environment variables..."

    local env_file="${PROJECT_DIR}/.env"
    local profile_snippet="${PROJECT_DIR}/hcps_env.sh"

    # Determine GnuCOBOL paths
    local COB_CONFIG_DIR=""
    local COB_COPY_DIR=""
    local COB_LIB_DIR=""

    if [ "$OS_TYPE" = "macos" ]; then
        local brew_prefix
        brew_prefix=$(brew --prefix 2>/dev/null || echo "/usr/local")
        COB_CONFIG_DIR="${brew_prefix}/share/gnucobol/config"
        COB_COPY_DIR="${brew_prefix}/share/gnucobol/copy"
        COB_LIB_DIR="${brew_prefix}/lib"
    else
        COB_CONFIG_DIR="/usr/share/gnucobol/config"
        COB_COPY_DIR="/usr/share/gnucobol/copy"
        COB_LIB_DIR="/usr/lib"
    fi

    # Create environment file
    cat > "$profile_snippet" << ENVEOF
#!/usr/bin/env bash
# HCPS Environment Configuration
# Source this file: source ${profile_snippet}

# GnuCOBOL configuration
export COB_CONFIG_DIR="${COB_CONFIG_DIR}"
export COB_COPY_DIR="${COB_COPY_DIR}"
export COBCPY="${PROJECT_DIR}/COPYBOOKS:\${COB_COPY_DIR}"

# Library path
export LD_LIBRARY_PATH="${COB_LIB_DIR}:\${LD_LIBRARY_PATH:-}"
export DYLD_LIBRARY_PATH="${COB_LIB_DIR}:\${DYLD_LIBRARY_PATH:-}"

# FreeTDS / Sybase configuration
export FREETDS_CONF="${PROJECT_DIR}/CONFIG/freetds.conf"
export SYBASE="${PROJECT_DIR}/CONFIG"

# HCPS project paths
export HCPS_HOME="${PROJECT_DIR}"
export HCPS_COBOL="${PROJECT_DIR}/COBOL"
export HCPS_COPYBOOKS="${PROJECT_DIR}/COPYBOOKS"
export HCPS_CONFIG="${PROJECT_DIR}/CONFIG"
export HCPS_BUILD="${PROJECT_DIR}/BUILD"
export HCPS_TEST="${PROJECT_DIR}/TEST"
export HCPS_LOG="${PROJECT_DIR}/LOG"

# Database connection (override as needed)
export HCPS_DB_SERVER="\${HCPS_DB_SERVER:-localhost}"
export HCPS_DB_PORT="\${HCPS_DB_PORT:-5000}"
export HCPS_DB_NAME="\${HCPS_DB_NAME:-hcps_dev}"
export HCPS_DB_USER="\${HCPS_DB_USER:-sa}"

# Add BUILD to PATH for compiled executables
export PATH="${PROJECT_DIR}/BUILD:\${PATH}"
ENVEOF

    chmod +x "$profile_snippet"
    log_pass "Environment file created: $profile_snippet"
    log_info "Source it with: source $profile_snippet"

    # Source it now
    # shellcheck disable=SC1090
    source "$profile_snippet"
}

#------------------------------------------------------------------------------
# Create Required Directories
#------------------------------------------------------------------------------
create_directories() {
    log_info "Creating project directory structure..."

    local dirs=(
        "${PROJECT_DIR}/COBOL"
        "${PROJECT_DIR}/COPYBOOKS"
        "${PROJECT_DIR}/JCL"
        "${PROJECT_DIR}/CONFIG"
        "${PROJECT_DIR}/SCRIPTS"
        "${PROJECT_DIR}/DATABASE"
        "${PROJECT_DIR}/BUILD"
        "${PROJECT_DIR}/TEST"
        "${PROJECT_DIR}/TEST/data"
        "${PROJECT_DIR}/TEST/output"
        "${PROJECT_DIR}/TEST/expected"
        "${PROJECT_DIR}/TEST/logs"
        "${PROJECT_DIR}/LOG"
        "${PROJECT_DIR}/DOC"
    )

    for dir in "${dirs[@]}"; do
        mkdir -p "$dir"
    done

    log_pass "Directory structure created"
}

#------------------------------------------------------------------------------
# Compile All COBOL Programs
#------------------------------------------------------------------------------
compile_programs() {
    if [ "$SKIP_COMPILE" = true ]; then
        log_info "Skipping compilation (--skip-compile)"
        return
    fi

    log_info "Compiling COBOL programs..."

    if [ -f "${SCRIPT_DIR}/compile_all.sh" ]; then
        chmod +x "${SCRIPT_DIR}/compile_all.sh"
        bash "${SCRIPT_DIR}/compile_all.sh"
    else
        log_warn "compile_all.sh not found; skipping compilation"
        log_info "You can compile manually with: cobc -x -I COPYBOOKS -o BUILD/PROGRAM COBOL/PROGRAM.cbl"
    fi
}

#------------------------------------------------------------------------------
# Verify Installation
#------------------------------------------------------------------------------
verify_installation() {
    log_info "Verifying installation..."

    local checks_passed=0
    local checks_total=0

    # Check GnuCOBOL
    checks_total=$((checks_total + 1))
    if command -v cobc &> /dev/null; then
        log_pass "GnuCOBOL compiler (cobc): FOUND"
        checks_passed=$((checks_passed + 1))
    else
        log_fail "GnuCOBOL compiler (cobc): NOT FOUND"
    fi

    # Check cobcrun
    checks_total=$((checks_total + 1))
    if command -v cobcrun &> /dev/null; then
        log_pass "GnuCOBOL runtime (cobcrun): FOUND"
        checks_passed=$((checks_passed + 1))
    else
        log_warn "GnuCOBOL runtime (cobcrun): NOT FOUND"
    fi

    # Check FreeTDS
    checks_total=$((checks_total + 1))
    if command -v tsql &> /dev/null; then
        log_pass "FreeTDS (tsql): FOUND"
        checks_passed=$((checks_passed + 1))
    else
        log_warn "FreeTDS (tsql): NOT FOUND (optional)"
        checks_passed=$((checks_passed + 1))
    fi

    # Check directory structure
    checks_total=$((checks_total + 1))
    if [ -d "${PROJECT_DIR}/COBOL" ] && [ -d "${PROJECT_DIR}/BUILD" ]; then
        log_pass "Directory structure: OK"
        checks_passed=$((checks_passed + 1))
    else
        log_fail "Directory structure: INCOMPLETE"
    fi

    # Check environment
    checks_total=$((checks_total + 1))
    if [ -f "${PROJECT_DIR}/hcps_env.sh" ]; then
        log_pass "Environment file: FOUND"
        checks_passed=$((checks_passed + 1))
    else
        log_fail "Environment file: NOT FOUND"
    fi

    # Simple compile test
    checks_total=$((checks_total + 1))
    local test_cob="/tmp/hcps_test_$$.cbl"
    cat > "$test_cob" << 'TESTEOF'
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCPSTEST.
       PROCEDURE DIVISION.
           DISPLAY "HCPS GnuCOBOL Test: OK".
           STOP RUN.
TESTEOF

    if cobc -x -o "/tmp/hcps_test_$$" "$test_cob" 2>/dev/null; then
        local output
        output=$("/tmp/hcps_test_$$" 2>&1)
        if [[ "$output" == *"OK"* ]]; then
            log_pass "GnuCOBOL compile+run test: PASS"
            checks_passed=$((checks_passed + 1))
        else
            log_fail "GnuCOBOL runtime test: FAIL"
        fi
        rm -f "/tmp/hcps_test_$$"
    else
        log_fail "GnuCOBOL compile test: FAIL"
    fi
    rm -f "$test_cob"

    echo ""
    echo "  Verification: $checks_passed / $checks_total checks passed"
    echo ""

    if [ "$checks_passed" -eq "$checks_total" ]; then
        log_pass "Setup complete. Environment is ready."
    else
        log_warn "Setup complete with warnings. Review messages above."
    fi
}

#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
main() {
    print_banner
    detect_os
    install_gnucobol
    install_sybase_client
    create_directories
    setup_environment
    compile_programs
    verify_installation

    echo ""
    echo "=================================================================="
    echo "  Next steps:"
    echo "    1. source ${PROJECT_DIR}/hcps_env.sh"
    echo "    2. cd ${PROJECT_DIR}"
    echo "    3. ./SCRIPTS/compile_all.sh"
    echo "    4. ./SCRIPTS/test_framework.sh"
    echo "=================================================================="
}

main "$@"
