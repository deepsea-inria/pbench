.PHONY: all install uninstall clean

all: prun_timeout

prun_timeout: prun_timeout.c
	gcc -Wno-implicit-function-declaration -o $@ $<

install: all
	install prun_timeout `opam config var prefix`/bin

uninstall:
	rm -f `opam config var prefix`/bin/prun_timeout

clean:
	rm -f prun_timeout
