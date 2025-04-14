#### Start of standard Makefile configuration. ####

SHELL := /usr/bin/env bash
LN_S := ln -sf

# Root of the installation
prefix := /usr/local

# Root of the executables
exec_prefix := $(prefix)

# Executables destination
bindir := $(exec_prefix)/bin

# Enable delete on error, which is disabled by default for legacy reasons
.DELETE_ON_ERROR:

#### End of standard Makefile configuration. ####

# Project specific absolute path
srcdir := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# Project specific variables
EXECUTABLE_NAME := cl-yasboi
EXECUTABLE_PATH := $(srcdir)/$(EXECUTABLE_NAME)
BUILD_SCRIPT := $(srcdir)/.internal-scripts/build-on-tmp-env.bash

# Lisp implementation (allow override, e.g., make LISP=ccl)
export LISP := sbcl

# Terminal colors for output
green := \\e[32m
blue := \\e[34m
bold := \\e[1m
reset := \\e[0m

.PHONY: all
all: install

.PHONY: build
build: $(EXECUTABLE_PATH)

$(EXECUTABLE_PATH): $(wildcard $(srcdir)/*.lisp $(srcdir)/*.asd)
	@$(BUILD_SCRIPT)

.PHONY: install
install: $(EXECUTABLE_PATH) installdirs
	@echo -e $(blue)Installing ...$(reset)
	@echo -e '   'Installing $(green)$(EXECUTABLE_NAME)$(reset) in $(green)$(DESTDIR)$(bindir)/$(reset)$(bold)$(EXECUTABLE_NAME)$(reset)
	@$(LN_S) $(EXECUTABLE_PATH) $(DESTDIR)$(bindir)/$(EXECUTABLE_NAME)
	@echo -e $(blue)Installing$(reset) $(green)DONE$(reset)

.PHONY: installdirs
installdirs:
	@echo -e $(blue)Creating directories ...$(reset)
	@mkdir -p $(DESTDIR)$(bindir)
	@echo -e '   'Creating directory $(green)$(DESTDIR)$(bindir)$(reset)
	@echo -e $(blue)Creating directories$(reset) $(green)DONE$(reset)

.PHONY: uninstall
uninstall:
	@echo -e $(blue)Uninstalling ...$(reset)
	@echo -e '   'Deleting file $(green)$(EXECUTABLE_NAME)$(reset) in $(green)$(DESTDIR)$(bindir)/$(reset)$(bold)$(EXECUTABLE_NAME)$(reset)
	@rm -f $(DESTDIR)$(bindir)/$(EXECUTABLE_NAME)
	@echo -e $(blue)Uninstalling$(reset) $(green)DONE$(reset)

.PHONY: clean
clean:
	@echo -e $(blue)Cleaning ...$(reset)
	@echo -e "Note: 'make clean' shouldn't be necessary because the build script cleans up after itself."
	@echo -e '   'Deleting executable $(green)$(srcdir)/$(reset)$(bold)$(EXECUTABLE_NAME)$(reset)
	@rm -f $(EXECUTABLE_PATH)
	@echo -e '   'Deleting temporary Quicklisp bootstrap file $(green)$(srcdir)/quicklisp.lisp$(reset)
	@rm -f $(srcdir)/quicklisp.lisp
	@echo -e '   'Deleting temporary Quicklisp directory $(green)$(srcdir)/.ql-tmp/$(reset)
	@rm -rf $(srcdir)/.ql-tmp
	@echo -e $(blue)Cleaning$(reset) $(green)DONE$(reset)
