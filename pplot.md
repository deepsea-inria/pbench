
example:

	make pplot
	make -C examples/basic fib
	prun -prog examples/basic/fib -algo recursive,cached -n 39,40 -runs 2
	pplot -x n -y exectime -series algo 
	pplot bar -x n -y exectime -series algo 
	pplot bar -x n -y exectime -chart algo 
	pplot bar -x n,algo -y exectime --vertical
	pplot scatter -x n -y exectime -series algo 
	pplot table -row n -col algo -cell exectime
	pplot table -row n -table algo -cell exectime 
	pplot table -row n,algo -cell exectime 


usage:

		pplot [TYPE] [OPTIONS_FOR_TYPE]
or

		pplot -type [TYPE] [OPTIONS_FOR_TYPE]


where type is among: bar, scatter and table

and where

options common to all:

   -width 6.0 
   -height 6.0 
   -title foo
   -input filename   (where to read results)
   -output filename  (where to plot results)


options common to "bar" and "scatter":
   -legendpos [topright|topleft|...]   (see R codes for legend or file legend.ml)
   -chart a,b,c 
   -series a,b,c 
   -group_by a
   -y a
   -ylabel foo
   -ymin 2.0
   -ymax 3.0
   --yzero
   --ylog


options for "scatter" only:

   -xlabel foo
   -xmin 2.0
   -xmax 3.0
   --xzero
   -drawline 0


options for "bar" only:

   -x_titles_dir [horizontal|vertical]  ==> --vertical is a shorthand [TODO]

