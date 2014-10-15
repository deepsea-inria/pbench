all: basic

PBENCH_PATH=../..

include $(PBENCH_PATH)/Makefile_common

DOCS=README.pdf README.html

fib:
	ocamlopt $(XLIB_INCLUDES) $(XLIB_LIBS) $(XLIB_SRC) nums.cmxa fib.ml -o $@

basic: basic.pbench
	ln -sf $< $@ 

doc: $(DOCS)

clean: pbench_clean
	rm -f $(DOCS) 
	rm -f basic fib
