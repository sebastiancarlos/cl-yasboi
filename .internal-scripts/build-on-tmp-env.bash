#! /usr/bin/env bash

# create the quicklisp temporary environment
source .internal-scripts/create-quicklisp-tmp-env.bash

EXECUTABLE_PATH="${PROJECT_ROOT}/${EXECUTABLE_NAME}"

# Build the executable using the temporary Quicklisp env
echo -e "${blue}Building executable ${green}${EXECUTABLE_NAME}${reset}${blue} using temporary environment...${reset}"
if ! "$LISP" $LISP_FLAGS --load "${SCRIPT_DIR}/build-on-tmp-env.lisp"; then
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
