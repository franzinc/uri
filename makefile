# Makefile for uri

DEVELOPERMODE := $(shell test -f ../dcl.dxl && echo t)

ifeq ($(DEVELOPERMODE),t)
LISP ?= ../lisp -I dcl.dxl
else
LISP ?= /fi/cl/10.1/bin/mlisp8
STANDALONE = t
endif

LISPARGS = -qq -batch

default:
	@echo there is no default rule in this makefile
	@exit 1

TMP = build.tmp

test: FORCE
	rm -f $(TMP) *.fasl
	echo '(require :tester)' >> $(TMP)
# with removal of -batch, this can be handy for debugging:
#	echo '(setq util.test:*break-on-test-failures* t)' >> $(TMP)
	echo '(setq excl::*break-on-warnings* t)' >> $(TMP)
#	echo '(setq *compile-verbose* t)' >> $(TMP)
#	echo '(setq *compile-print* t)' >> $(TMP)
	echo '(load (compile-file "uri.cl"))' >> $(TMP)
	echo '(load (compile-file "t-uri.cl"))' >> $(TMP)
###### ONLY works in dcl.dxl until bug25662 is fixed
# with-tests sets this from util.test:*test-errors*:
	echo '(exit test::.total-errors.)' >> $(TMP)
	$(LISP) +s $(TMP) $(LISPARGS)

bm: FORCE
	rm -f $(TMP) *.fasl
ifeq ($(STANDALONE),t)
	echo '(load (compile-file "uri.cl"))' >> $(TMP)
endif
	echo '(load (compile-file "bench.cl"))' >> $(TMP)
	echo '(run-bms)' >> $(TMP)
	echo '(exit 0)' >> $(TMP)
	$(LISP) +s $(TMP) $(LISPARGS)

profile: FORCE
	rm -f $(TMP) *.fasl
ifeq ($(STANDALONE),t)
	echo '(load (compile-file "uri.cl"))' >> $(TMP)
endif
	echo '(load (compile-file "bench.cl"))' >> $(TMP)
	echo '(profile)' >> $(TMP)
	echo '(exit 0)' >> $(TMP)
	$(LISP) +s $(TMP) $(LISPARGS)

PREAMBLE = echo TESTED: $(shell hostname) on $(shell date);

bench: FORCE
	($(PREAMBLE) $(MAKE) $(MFLAGS) bm)      > results.bm.txt
	[ "$(PROFILE)" = "t" ] && \
	($(PREAMBLE) $(MAKE) $(MFLAGS) profile) > results.profile.txt

# The Lisp to use for reference benchmarks, on jesse.
LISPREF = /acl/layer/tmp/acl10.1/mlisp8

# We clean before the reference benchmark runs so the locally
# compiled version doesn't get loaded.  We want the version in the
# reference distribution.
benchref: clean
	($(PREAMBLE) $(MAKE) $(MFLAGS) LISP=$(LISPREF) bm) \
	    > results.bm.reference.txt
	[ "$(PROFILE)" = "t" ] && \
	($(PREAMBLE) $(MAKE) $(MFLAGS) LISP=$(LISPREF) profile) \
	    > results.profile.reference.txt

all: export PROFILE = t
all: bench benchref

clean: FORCE
	rm -f *.fasl

FORCE:
