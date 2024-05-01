.PHONY: all
all:
	@ dune build

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: install
install:
	@ dune install --display=short

.PHONY: uninstall
uninstall:
	@ dune uninstall --display=short
