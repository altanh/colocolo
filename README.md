# Colocolo

Colocolo provides an embedding of relational logic in 
[Rosette](https://emina.github.io/rosette), a solver-aided programming language. It is built on top of [Ocelot](https://github.com/jamesbornholt/ocelot/), implementing a few key optimizations, most notably skolemization, Colocolo enables both verification
and synthesis of relational logic expressions.

## Installation

Clone this respository, enter its directory, and run:

    raco pkg install

In order for Colocolo to be able to invoke lingeling (our SAT solver of choice), you must add [lingeling](https://github.com/arminbiere/lingeling) to your PATH. If you wish to use a different SAT solver you can modify the process call in `engine/sat/solver.rkt`.

## Running the Benchmarks

After installing Colocolo, navigate to colocolo/bench. `runtests` will run all benchmarks and print out results, `runtests-process` will run all benchmarks and print out total time, solver time, and completed to 3 csv files. Completed is there because we set a 30 second timeout for our calls to lingeling, so we wanted to know which timed out. Note `runtests-process` **requires Python 3** to be installed.

So either run:

    ./runtests
or

    ./runtests-process results.txt

You may need to add executable permissions:

    chmod +x runtests
    chmod +x runtests-process
Of course any individual benchmark can be run:

    racket <benchmark>.rkt

Each benchmark is in the form `<benchmark>(-<optimization>)*.rkt` representing every combination of optimizations on a given benchmark.

If this fails on SAT invocations, make sure you have added [lingeling](https://github.com/arminbiere/lingeling) to your PATH.

## Compiling Kodkod Benchmarks with Meow

In order to compile Kodkod instances (in Java), go to [Meow](https://github.com/altanh/meow) and follow the instructions. To get a Kodkod instance of any Alloy problem (`*.als`), paste it into Alloy and run it making sure that `Options > record the Kodkod Input/Output:` is toggled to `Yes`.


    

