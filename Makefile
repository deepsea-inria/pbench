all: doc

PBENCH_PATH=.

include Makefile_common

DOCS=README.pdf README.html

doc: $(DOCS)

clean: pbench_clean
	rm -f $(DOCS) 

