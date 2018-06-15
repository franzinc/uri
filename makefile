# Makefile for uri

ifdef REFERENCE
# use mlisp8 so it's comparable to dcl.dxl
LISP = /fi/cl/10.1/bin/mlisp8-64 -qq -batch
else
LISP = ../lisp -I dcl.dxl -qq -batch
endif

default:
	@echo there is no default rule in this makefile
	@exit 1

TMP = build.tmp

test: FORCE
	rm -f $(TMP) *.fasl
# with removal of -batch, this can be handy for debugging:
#	echo '(setq util.test:*break-on-test-failures* t)' >> $(TMP)
	echo '(setq excl::*break-on-warnings* t)' >> $(TMP)
#	echo '(setq *compile-verbose* t)' >> $(TMP)
#	echo '(setq *compile-print* t)' >> $(TMP)
	echo '(load (compile-file "uri.cl"))' >> $(TMP)
	echo '(load (compile-file "t-uri.cl"))' >> $(TMP)
# with-tests sets this from util.test:*test-errors*:
	echo '(exit test::.total-errors.)' >> $(TMP)
	$(LISP) +s $(TMP)

bm: FORCE
	rm -f $(TMP) *.fasl
	echo '(load (compile-file "bench.cl"))' >> $(TMP)
	echo '(run-bms)' >> $(TMP)
	echo '(exit 0)' >> $(TMP)
	$(LISP) +s $(TMP)

profile: FORCE
	rm -f $(TMP) *.fasl
	echo '(load (compile-file "bench.cl"))' >> $(TMP)
	echo '(profile)' >> $(TMP)
	echo '(exit 0)' >> $(TMP)
	$(LISP) +s $(TMP)

PREAMBLE = echo TESTED: $(shell hostname) on $(shell date);

bench: FORCE
	($(PREAMBLE) make bm)                  > results.bm.txt
	($(PREAMBLE) make profile)             > results.profile.txt

benchref: FORCE
	($(PREAMBLE) make REFERENCE=t bm)      > results.bm.reference.txt
	($(PREAMBLE) make REFERENCE=t profile) > results.profile.reference.txt

all: bench benchref

FORCE:
