Parallel algorithm benchmarking toolkit: pbench
===============================================

Dependencies
------------

This source files of this package consist mostly of
ocaml code.

The build system requires that version >= 4.00 of
[ocaml](http://www.ocaml.org/) be installed and also
a recent version of the gnu c compiler 
[GCC](http://gcc.gnu.org/).

The run and plot tools
----------------------

These tools are still under development. At present,
only custom ocaml scripts are supported.

Getting started with writing custom benchmarking scripts
--------------------------------------------------------

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



