all: prun 
# pplot

PBENCH_PATH=.

include Makefile_common

DOCS=README.pdf README.html

prun: prun.pbench
	ln -sf $< $@ 

pplot: pplot.pbench
	ln -sf $< $@ 

doc: $(DOCS)

clean: pbench_clean
	rm -f $(DOCS) 
	rm -f prun pplot
