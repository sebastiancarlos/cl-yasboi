#! /usr/bin/env bash

# Exit immediately if a command exits with a non-zero status.
set -e

# Set SCRIPT_DIR to the directory where this script is localed.
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

PROJECT_ROOT="$( cd "$SCRIPT_DIR/.." &> /dev/null && pwd )"

QL_TMP_DIR="${PROJECT_ROOT}/.ql-tmp"
QL_TMP_SETUP="${QL_TMP_DIR}/setup.lisp"
QL_TMP_BOOTSTRAP="${PROJECT_ROOT}/quicklisp.lisp"
QL_BOOTSTRAP_URL="https://beta.quicklisp.org/quicklisp.lisp"

# Project specific
EXECUTABLE_NAME="cl-yasboi"
EXECUTABLE_PATH="${PROJECT_ROOT}/${EXECUTABLE_NAME}"

# Lisp implementation command (can be overridden by environment variable)
LISP_CMD="${LISP:-sbcl}"

# SBCL flags for non-interactive execution
LISP_FLAGS="--non-interactive --noprint"

# Output formatting
green=$'\e[32m'
blue=$'\e[34m'
red=$'\e[31m'
bold=$'\e[1m'
reset=$'\e[0m'

error() {
  echo -e "${bold}${red}Error:${reset} ${bold}$*${reset}" >&2;
  exit 1;
}

cleanup() {
    echo -e "${blue}Cleaning up temporary build environment...${reset}"
    echo -e "    Removing temporary Quicklisp directory ${green}.ql-tmp/${reset}"
    rm -f "$QL_TMP_BOOTSTRAP" &> /dev/null
    echo -e "    Removing temporary Quicklisp setup file ${green}setup.lisp${reset}"
    rm -rf "$QL_TMP_DIR" &> /dev/null
    echo -e "${blue}Cleaning up temporary build environment${reset} ${green}DONE${reset}"
}

# Register cleanup function to run on script exit (normal or error)
trap cleanup EXIT

# --- Main Logic ---

# Check prerequisites
echo -e "${blue}Checking prerequisites...${reset}"
if ! command -v "$LISP_CMD" &> /dev/null; then
    error "'$LISP_CMD' command not found. Please install SBCL or set the LISP environment variable."
fi
echo -e "    ${bold}${LISP_CMD}${reset} ${green}OK${reset}"
if ! command -v curl &> /dev/null; then
    error "'curl' command not found. Please install curl."
fi
echo -e "    ${bold}curl${reset} ${green}OK${reset}"
echo -e "${blue}Checking prerequisites${reset} ${green}DONE${reset}"

cd "$PROJECT_ROOT"

# Download Quicklisp bootstrap file
echo -e "${blue}Downloading Quicklisp bootstrap file...${reset}"
if ! curl -fLo "${QL_TMP_BOOTSTRAP}" "${QL_BOOTSTRAP_URL}" &> /dev/null; then
    error "Failed to download Quicklisp bootstrap file from ${QL_BOOTSTRAP_URL}"
fi
echo -e "${blue}Downloading Quicklisp bootstrap file${reset} ${green}DONE${reset}"

# Install Quicklisp temporarily
echo -e "${blue}Installing Quicklisp temporarily into ${green}.ql-tmp/${reset}${blue}...${reset}"
if ! "$LISP_CMD" $LISP_FLAGS --load "${SCRIPT_DIR}/install-quicklisp-on-tmp-env.lisp" > /dev/null; then
    error "Temporary Quicklisp installation failed."
fi
if [ ! -f "$QL_TMP_SETUP" ]; then
    error "Lisp script succeeded, but $QL_TMP_SETUP was not created."
fi
echo -e "${blue}Installing Quicklisp temporarily into ${green}.ql-tmp/${reset}... ${green}DONE${reset}"

# Build the executable using the temporary Quicklisp env
echo -e "${blue}Building executable ${green}${EXECUTABLE_NAME}${reset}${blue} using temporary environment...${reset}"
if ! "$LISP_CMD" $LISP_FLAGS --load "${SCRIPT_DIR}/make-executable-on-tmp-env.lisp" > /dev/null; then
    error "Build process failed."
fi
if [ ! -f "$EXECUTABLE_PATH" ]; then
    error "Build command succeeded, but the executable ${EXECUTABLE_PATH} was not found."
fi
echo -e "${blue}Building executable ${green}${EXECUTABLE_NAME}${reset}${blue} using temporary environment${reset} ${green}DONE${reset}"

# info "Build complete. Executable created at ${EXECUTABLE_PATH}"
echo -e "${blue}Build complete. Executable ${green}${EXECUTABLE_NAME}${reset}${blue} created.${reset}"

# Cleanup happens automatically on exit
exit 0
