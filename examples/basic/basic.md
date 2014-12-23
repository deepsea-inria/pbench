% Example Benchmarking Script User's Guide
% [Umut Acar](http://www.umut-acar.org/); 
  [Arthur Charguéraud](http://www.chargueraud.org/); 
  [Mike Rainey](http://gallium.inria.fr/~rainey/)
% 17 September 2014

Synopsis
========

basic [*ACTION*] [*PARAMETERS*]...

Description
===========

basic is a script whose purpose is to provide an example of how
to write benchmarking scripts using the `pbench` library. This script
performs a small performance study of an OCaml program, namely
`fib.ml`, that implements two alternative algorithms for computing the
$n^{th}$ Fibonacci number. There is one doubly recursive algorithm
titled `recursive` and one using cached binary recursion titled
`cached`. The basic script automates the building of the
`fib.byte` binary, the running of the experiments, the consistency
checks, and plot generation.

Options
=======

Actions
-------

The action selects the overall behavior of the script.
*ACTION* can be one of the following:

`fib`
:    Run the `fib` experiment.

Parameters
----------

Parameters select finer details of the behavior of the script.
*PARAMETERS* can be zero or more of the following:

`-runs` *n*
:   Specifies the number of times *n* to execute for each combination of 
    benchmark parameters. Default is 1.

`-timeout` *n*
:   Force a specific timeout for runs. The timeout value *n* is measured
    in seconds.

`--virtual_run`
:   Only show the list of commands that would be called to run the benchmarks.

`--virtual_build`
:   Only show the list of commands that would be called to build the benchmarks.

`-skip` *a1*,*a2*,...
:   Skip selected actions. Note: `-skip run` automatically activates
    `-skip make`.

`-only` *a1*,*a2*,...
:   Perform only selected actions.

Sample applications
===================

See commands to be executed
---------------------------

    basic fib --virtual_run

Build, run, then plot
---------------------

In this experiment, we run each command five times and report the
average.

    basic fib -runs 5

Just plot
---------

This command succeeds only if there was a previous run and there
exists a corresponding results file named `results_fib.txt` that is in
the current directory.

    basic fib -only plot

Just run
--------

    basic fib -skip make,plot