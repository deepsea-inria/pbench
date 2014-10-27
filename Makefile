all: prun pplot

PBENCH_PATH=.

include Makefile_common

DOCS_KEYS=README prun pplot
DOCS=$(DOCS_KEYS:=.html) $(DOCS_KEYS:=.pdf)

prun: prun.pbench
	ln -sf $< $@ 

pplot: pplot.pbench
	ln -sf $< $@ 

doc: $(DOCS)
doc_html: $(DOCS_KEYS:=.html)

clean: pbench_clean
	rm -f $(DOCS) 
	rm -f prun pplot
