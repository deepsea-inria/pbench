

demo:
	make prun
	make -C examples/basic fib
	prun -prog examples/basic/fib -algo recursive,cached -n 39,40 -runs 2

	prun -prog examples/others/speedup.sh -proc 0,1,2,3,4
	prun -prog examples/others/speedup.sh -proc 0,1,2,3,4 -algo foo,bar


usage

	prun [PRUN_OPTIONS] [PROG_OPTIONS] 

order of all options is not relevant

or

	prun [PRUN_OPTIONS] -args "[PROG_OPTIONS]" 

remark: the later form should be used if you want stability through updates of pbench that may add new options.



PRUN_OPTIONS

 --verbose
 --virtual
 --dummy  
 -runs 3  (default is 1)
 -timeout 10   (in seconds, must be int > 0; or use -1 for no timeout (-1 is default))
 -output  filename  (default "results.txt")
 -attempts 2  (max nb of attempts when runs fail; default is 1)
 -args "[PROG_OPTIONS]" 
 --append  
 --complete  (currently incompatible with -runs)
 --replace   (currently incompatible with -runs)
 -mode [normal|append|complete|replace]   (like the options above)

PROG_OPTIONS

should be in the form

	prog1,prog2  -n 34,35  -m 3.2,4.5
or
	-prog prog1,prog2  -n 34,35  -m 3.2,4.5

the cross product of all coma separated values is considered



remarks:
  the result of every run is recorded in the folder _results/