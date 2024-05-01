.PHONY: all
all:
	@ dune build .

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: test
test:
	@ dune build .
	@ ln -sf ../../_build/default/examples/basic/fib.exe ./fib
	@ dune exec ./basic.exe
