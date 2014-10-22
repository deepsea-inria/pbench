pbench: a parallel algorithm benchmarking toolkit
=================================================

The `pbench` toolkit is a software framework whose purpose is
to help programmers benchmark their programs, especially 
parallel programs. The toolkit provides two command-line tools,
namely `prun` and `pplot`, that are useful for small, explorative
benchmarking scenarios. The `prun` tool organizes and executes
measured runs of client-supplied benchmark programs. Crashes
and timeouts of benchmark programs are handled gracefully
by `prun`. The `pplot` tool generates various human-readable
output from the data collected by `prun` experiments. Forms
of output include latex tables, R bar and scatter plots, etc.
Data that is output by `pplot` can be lightly processed,
for example, to show the mean run time of a number of runs.
Moreover, useful data, such as error bars, can be rendered
by `pplot`.

Also provided by `pbench` is an Ocaml library that is useful for 
writing sophisticated performance evaluations of one or more 
benchmark programs. The goal of the library is to assist
experimentalists who want to make their experimental evaluations
repeatable by other experimenters. To this end, we provide
a few example programs that show how one can use our library
to automate all or nearly all of the experimental evaluation
of a research paper.

Note that the benchmark programs themselves can be written in
any language. The only requirement is that the benchmark programs
honor a certain format for receiving command-line arguments
and printing measurements to `stdout`.

Software dependencies
---------------------

The build system requires that version >= 4.00 of
[ocaml](http://www.ocaml.org/) be installed and also
a recent version of the gnu c compiler 
[GCC](http://gcc.gnu.org/).

The command-line tools
----------------------

The documentation of the `prun` tool can be found
in `prun.md` and that of the `pplot` tool in `pplot.md`.

Writing custom benchmarking scripts
-----------------------------------

We have created an example script to demonstrate basic use
of our library. The script (and the rest of our library)
is written in [ocaml](http://www.ocaml.org/). At least
an intermediate level of expertise in ocaml is essential
to write custom scripts.

The example script is named `example.ml` and can be found
in the top-level folder of the pbench source tree. 
The associated file named `example.md` provides documentation
of the command-line usage of the script. Although the
source of the document is suitable for reading, the document
can alternatively be rendered and read as a PDF file by the 
following command.

    make example.pdf

To build the ocaml script, namely `example.byte`, make the
following build command.

    make example.byte
    
The documentation explains the usage of the program.

For more advanced examples, see `chunkedseq.ml` and 
`graph.ml`.



