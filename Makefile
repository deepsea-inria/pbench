
SRC=$(filter-out bin.ml,$(wildcard *.ml) $(wildcard xlib/*.ml) $(wildcard parsing/*.ml))

all: demo timeout.out

timeout.out: timeout.c
	gcc -o $@ $<

%.byte: $(SRC)
	ocamlbuild -quiet -I parsing -I xlib -lib unix -lib str -lib nums $@

%.pdf : %.md
	pandoc $< -s -o $@
%.html : %.md
	pandoc $< -s -o $@

demo: demo.byte

example: example.byte
fib: fib.byte

pbench: pbench.byte
	@cp pbench.byte pbench

run: run.byte
	cp run.byte run

plot: plot.byte
	cp plot.byte plot

doc: graph.pdf chunkedseq.pdf example.pdf

clean:
	ocamlbuild -clean
	rm -f *.cmi *.cmo *.cma *.o *.cmx *.byte pbench
	rm -f parsing/*.cmi parsing/*.cmo parsing/*.cma parsing/*.o parsing/*.cmx
	rm -f xlib/*.cmi xlib/*.cmo xlib/*.cma xlib/*.o xlib/*.cmx
	rm -f parsing/lexer.ml parsing/parser.ml parsing/parser.mli

# parsing/ast_mapper.ml
FILES=xlib/XBase.ml xlib/XList.ml xlib/XCmd.ml xlib/XFile.ml  xlib/XMath.ml xlib/XOption.ml xlib/XString.ml parsing/misc.mli parsing/misc.ml parsing/longident.mli parsing/longident.ml parsing/warnings.mli parsing/warnings.ml parsing/terminfo.mli parsing/terminfo.ml parsing/location.mli parsing/location.ml parsing/asttypes.mli parsing/parsetree.mli  parsing/ast_helper.mli  parsing/ast_helper.ml   parsing/syntaxerr.mli parsing/syntaxerr.ml parsing/parser.mli parsing/parser.ml parsing/lexer.mli parsing/lexer.ml  parsing/parse.mli  parsing/parse.ml    syntax.ml env.ml  convert.ml  semantics.ml  sys_tools.ml run_tools.ml  plot_tools.ml   fcts.ml  core_fcts.ml run_fcts.ml  plot_fcts.ml pbench_fcts.ml pbench.ml

MLIFILES=unix.cma str.cma parsing/longident.mli parsing/warnings.mli parsing/location.mli parsing/asttypes.mli parsing/parsetree.mli  parsing/ast_helper.mli parsing/misc.mli parsing/terminfo.mli parsing/syntaxerr.mli parsing/syntaxerr.ml parsing/parser.mli parsing/parser.ml parsing/lexer.mli  parsing/parse.mli  parsing/pprintast.mli


# ocamlyacc

alt:
	OCAMLLIB=~/ocamleasy/ocaml/stdlib; ocamlbuild -ocamlc ~/ocamleasy/ocaml/ocamlc -no-stdlib  -I parsing -I xlib -lib unix -lib str  pbench.byte
	#-I /home/charguer/ocamleasy/ocaml/otherlibs/unix -I /home/charguer/ocamleasy/ocaml/otherlibs/str -I ~/ocamleasy/ocaml/stdlib

parsing/lexer.ml:
	ocamllex parsing/lexer.mll

parsing/parser.ml:
	ocamlyacc parsing/parser.mly

#parsing/parser.mli:
#	ocamlyacc parsing/parser.mly

type_parser: parsing/lexer.ml parsing/parser.ml # parsing/parser.mli



type: type_parser
	@OCAMLLIB=~/ocamleasy/ocaml/stdlib; ~/ocamleasy/ocaml/ocamlopt.opt -easytype -nostdlib -I ~/ocamleasy/ocaml/stdlib -I ~/ocamleasy/ocaml/otherlibs/unix -I ~/ocamleasy/ocaml/otherlibs/str -I parsing -I xlib unix.cmxa str.cmxa $(FILES) -o pbench
	@echo Compilation succeeded.

%.cmo: %.ml
	OCAMLLIB=~/ocamleasy/ocaml/stdlib; ~/ocamleasy/ocaml/ocamlopt.opt -nostdlib -I ~/ocamleasy/ocaml/stdlib -I ~/ocamleasy/ocaml/otherlibs/unix -I ~/ocamleasy/ocaml/otherlibs/str -I parsing -I xlib unix.cma str.cma $(MLIFILES) $< -o $@




#-nostdlib -I



# go: all
#	./pbench.byte


#OCAML_LIBS=unix.cmxa str.cmxa

#OCAML_LIBSA=

#	OCAMLLIB=~/ocamleasy/ocaml/stdlib; ocamlbuild -ocamlc ~/ocamleasy/ocaml/ocamlc.opt -I parsing -I xlib -lib unix -lib str run.byte

# 	ocamlc unix.cma str.cma XBase.ml XOption.ml XList.ml XString.ml XFile.ml XMath.ml XCmd.ml Run_tools.ml pbench.ml -o pbench.out



#  make && ./pbench.byte "[1;3]*(10**(5@9))"
#  make && ./pbench.byte "cross_params(z=[0;1], n=10**8, x=10**(3@8), y=n/x)"

# make && ./pbench.byte "cross_params(x=1)"
# make && ./pbench.byte "cross_params(x=1,y=[2;3],z=x/2)"

#make && ./pbench.byte "params(a=[0;1], b=[2;3]; c=2*a)"

#make && ./pbench.byte "(a=0) & (b=1)"

#==> sample(nb,a,b);  sample_log(nb,a,b)

# make && ./pbench.byte "run(prog="test.exe", args=(a=0 & b=[a,1]), timeout=3)"
# make && ./pbench.byte "run(prog=\"test.exe\", args=(x=4 & y=[5;x]), timeout=3)"

#  make && ./pbench.byte "run(prog=\"test.sh\", args=(x=4 & y=[5;x]), timeout=3, runs=3)"

##  make run &&  ./run -timeout 3 -runs 3 test.sh,test2.sh x=4,5,6 y=100/x --append -attemps 4
##  make run &&  ./run -timeout 3 -runs 3 test.sh x=4,5,6 y=100/x --append
##  make run &&  ./run -timeout 3 -runs 3 test.sh -n 100 -x 4,5,6 -y n/x --append

#  make && ./pbench.byte "results_from_file(\"results.txt\",\"foo.txt\")"

#  make && ./pbench.byte "scatter_plot(results=[\"test_results.txt\";\"test_baseline.txt\"], charts=(foo=[0;1]), series=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=4))), x=(key=[1;2]), y=exectime, legend=\"bottomright\", yzero=1)"

#  make && ./pbench.byte "scatter_plot(results=[\"test_results.txt\";\"test_baseline.txt\"], charts=(foo=[0;1]), series=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=4))), x=(key=[1;2]), y=exectime/(mean_of(results_values(\"exectime\", results_select(_all_results, prog=\"base.sh\" & x=x)))), legend=\"bottomright\", yzero=1)"


#  make && ./pbench.byte "scatter_plot(results=\"test_results.txt\", charts=(foo=[0;1]), series=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=4))), x=(key=[1;2]), y=exectime/(mean_of(results_values(\"exectime\", results_select(\"test_baseline.txt\", prog=\"base.sh\" & key=key)))), legend=\"bottomright\", yzero=1)"

## here is the full demo:

#  make && ./pbench.byte "scatter_runplot(results=\"results.txt\", charts=(foo=[0;1]), series=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=4))), x=(key=[1;2]), y=exectime/(mean_of(results_values(\"exectime\", results_select(\"test_baseline.txt\", prog=\"base.sh\" & key=key)))), legend=\"bottomright\", yzero=1, runs=2)"



#  make && ./pbench.byte "bar_plot(results=[\"test_results.txt\";\"test_baseline.txt\"], charts=(foo=[0;1]), series=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=4))), x=(key=[1;2]), y=exectime)"

#  make && ./pbench.byte "bar_plot(results=[\"test_results.txt\";\"test_baseline.txt\"], series=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=4))), x=(key=[1;2] & foo=[0;1]), y=exectime, label_rotation=\"vertical\")"
#  make && ./pbench.byte "bar_plot(results=[\"test_results.txt\";\"test_baseline.txt\"], series=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=4))), x=(key=[1;2] & foo=[0;1]), y=exectime, label_rotation=\"horizontal\")"




#  make && ./pbench.byte "bar_runplot(results=\"results.txt\", x=(((prog=\"test.sh\") ++ (prog=\"test2.sh\" & param=[4;5]))), series=(key=[1;2] & foo=[0;1]), y=exectime/(mean_of(results_values(\"exectime\", results_select(\"test_baseline.txt\", prog=\"base.sh\" & key=key)))), yzero=1, runs=2, label_rotation=\"vertical\")"


# make clean
# make && ./pbench -open test_script.pbh "xval"
# make && ./pbench -open test_script.pbh "xfun()"
# make && ./pbench -open test_script.pbh "bfs()"
# make type && ./pbench -open test_script.pbh "bfs()"
