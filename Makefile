
PBENCH_PATH=.

include Makefile_common

DOCS=README.pdf README.html

all: $(TIMEOUT)

doc: $(DOCS)

clean: pbench_clean
	rm -f $(DOCS) 

