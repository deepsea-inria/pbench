

	make pplot
	make -C examples/basic fib
	prun -prog examples/basic/fib -algo recursive,cached -n 39,40 -runs 2
	pplot -x n -y exectime -series algo 

