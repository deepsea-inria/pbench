
PBENCH_PATH=.

BINARIES=prun pplot prun_timeout

all: $(BINARIES)


##################################################################
# C Binaries

TIMEOUT_SCR=prun_timeout.c
TIMEOUT=prun_timeout

$(TIMEOUT): $(TIMEOUT_SCR)
	gcc -Wno-implicit-function-declaration -o $@ $<


##################################################################
# OCaml Binaries

include Makefile_common

prun: prun_timeout

prun: prun.pbench
	cp $< $@ 

pplot: pplot.pbench
	cp $< $@ 


##################################################################
# Documentation

DOCS_KEYS=README prun pplot
DOCS=$(DOCS_KEYS:=.html) $(DOCS_KEYS:=.pdf)

doc: $(DOCS)
doc_html: $(DOCS_KEYS:=.html)


##############################################################################
# Cleaning

clean: pbench_clean
	rm -f $(DOCS) 
	rm -f $(BINARIES)


##############################################################################
# Installation in opam prefix or in /usr/local/bin folder

PREFIX := $(shell opam config var prefix)/bin
ifeq ($(PREFIX),)
	PREFIX := /usr/local/bin
endif

install: all
	install $(BINARIES) $(PREFIX)

uninstall:
	rm -f $(BINARIES)



