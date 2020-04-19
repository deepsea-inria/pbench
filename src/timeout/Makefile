.PHONY: all install clean

all: prun_timeout

install: all
	install prun_timeout `opam config var prefix`/bin

prun_timeout: prun_timeout.c
	gcc -Wno-implicit-function-declaration -o $@ $<

clean:
	rm -f prun_timeout
