.PHONY: all install uninstall clean

all:
	dune build @install
	make -C src/timeout $@

install:
	dune install
	make -C src/timeout $@

uninstall:
	dune uninstall

clean:
	dune clean
	make -C src/timeout clean
