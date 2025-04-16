# Exit immediately if a command exits with a non-zero status.
set -e

# Set SCRIPT_DIR to the directory where this script is localed.
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

PROJECT_ROOT="$( cd "$SCRIPT_DIR/.." &> /dev/null && pwd )"

QL_TMP_DIR="${PROJECT_ROOT}/.ql-tmp"
QL_TMP_SETUP="${QL_TMP_DIR}/setup.lisp"
QL_TMP_BOOTSTRAP="${PROJECT_ROOT}/quicklisp.lisp"
QL_BOOTSTRAP_URL="https://beta.quicklisp.org/quicklisp.lisp"

LISP_FLAGS="--noinform --non-interactive --no-userinit --noprint"

error() {
  echo -e "${bold}${red}Error:${reset} ${bold}$*${reset}" >&2;
  exit 1;
}

cleanup() {
    if [[ "${CI:-}" == "true" ]]; then
        echo -e "Running in CI environment. Preserving ${green}.ql-tmp/${reset} for caching."
        return
    fi

    echo -e "${blue}Cleaning up temporary build environment...${reset}"
    echo -e "    Removing temporary Quicklisp directory ${green}.ql-tmp/${reset}"
    rm -f "$QL_TMP_BOOTSTRAP" &> /dev/null
    echo -e "    Removing temporary Quicklisp setup file ${green}setup.lisp${reset}"
    rm -rf "$QL_TMP_DIR" &> /dev/null
    echo -e "${blue}Cleaning up temporary build environment${reset} ${green}DONE${reset}"
}

# Register cleanup function to run on script exit (normal or error)
trap cleanup EXIT

# Check prerequisites
echo -e "${blue}Checking prerequisites...${reset}"
if ! command -v "$LISP" &> /dev/null; then
    error "'$LISP' command not found. Please install SBCL or set the LISP environment variable."
fi
echo -e "    ${bold}${LISP}${reset} ${green}OK${reset}"
if ! command -v curl &> /dev/null; then
    error "'curl' command not found. Please install curl."
fi
echo -e "    ${bold}curl${reset} ${green}OK${reset}"
echo -e "${blue}Checking prerequisites${reset} ${green}DONE${reset}"

create-quicklisp-tmp-env() {
  if [[ -d "$QL_TMP_DIR" ]]; then
      echo -e "${blue}Temporary Quicklisp directory ${green}.ql-tmp${reset} already exists. Assuming it's a restored cache on CI."
      return 
  fi

  # Download Quicklisp bootstrap file
  cd "$PROJECT_ROOT"
  echo -e "${blue}Downloading Quicklisp bootstrap file...${reset}"
  if ! curl -fLo "${QL_TMP_BOOTSTRAP}" "${QL_BOOTSTRAP_URL}"; then
      error "Failed to download Quicklisp bootstrap file from ${QL_BOOTSTRAP_URL}"
  fi
  echo -e "${blue}Downloading Quicklisp bootstrap file${reset} ${green}DONE${reset}"

  # Install Quicklisp temporarily
  echo -e "${blue}Installing Quicklisp temporarily into ${green}.ql-tmp/${reset}${blue}...${reset}"
  if ! "$LISP" $LISP_FLAGS --load "${SCRIPT_DIR}/install-quicklisp-on-tmp-env.lisp"; then
      error "Temporary Quicklisp installation failed."
  fi
  if [ ! -f "$QL_TMP_SETUP" ]; then
      error "Lisp script succeeded, but $QL_TMP_SETUP was not created."
  fi
  echo -e "${blue}Installing Quicklisp temporarily into ${green}.ql-tmp/${reset}${blue}... ${green}DONE${reset}"
}
create-quicklisp-tmp-env
