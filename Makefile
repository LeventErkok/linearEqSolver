# (c) Copyright Levent Erkok. All rights reserved.
#
# The linearEqSolver library is distributed with the BSD3 license. See the LICENSE file
# in the distribution for details.
SHELL     := /usr/bin/env bash
TSTSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' | grep -v Setup.hs)
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_linearEqSolver.hs)
CABAL     = cabal
TIME      = /usr/bin/time

define mkTags
	@find . -name \*.\*hs | xargs fast-tags
endef

.PHONY: all install sdist clean docs hlint tags

all: install

install: $(DEPSRCS) Makefile
	$(call mkTags)
	@$(CABAL) new-install --lib --force-reinstall

test: install
	@echo "*** Starting inline tests.."
	@(set -o pipefail; $(TIME) doctest -package sbv -package base ${TSTSRCS} 2>&1)

sdist: install
	@(set -o pipefail; $(CABAL) new-sdist)

veryclean: clean

clean:
	@rm -rf dist dist-newstyle

docs:
	@(set -o pipefail; $(CABAL) new-haddock --haddock-option=--no-warnings 2>&1)

release: clean install sdist hlint test docs
	@echo "*** LinearEqSolver is ready for release!"

hlint: install
	@echo "Running HLint.."
	@hlint Math -i "Use otherwise" -i "Parse error"

tags:
	$(call mkTags)
