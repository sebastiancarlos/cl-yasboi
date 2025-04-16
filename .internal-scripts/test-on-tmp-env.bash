#! /usr/bin/env bash

# create the quicklisp temporary environment
source .internal-scripts/create-quicklisp-tmp-env.bash

# --- Main Logic ---

# Build the executable using the temporary Quicklisp env
echo -e "${blue}Running Test Suite...${reset}"
if ! "$LISP" $LISP_FLAGS --load "${SCRIPT_DIR}/test-on-tmp-env.lisp"; then
    error "Test suite failed."
fi
echo -e "${blue}Running Test Suite${reset} ${green}DONE${reset}"

# Cleanup happens automatically on exit
exit 0
