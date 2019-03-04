# HTAR9 Assembler and Interpreter
[![pipeline status](https://gitlab.com/BorisNikulin/htar9-asm/badges/master/pipeline.svg)](https://gitlab.com/BorisNikulin/htar9-asm/commits/master)
[![coverage report](https://gitlab.com/BorisNikulin/htar9-asm/badges/master/coverage.svg)](https://borisnikulin.gitlab.io/htar9-asm/coverage/)

## Builds
There are two different build versions. The first is a mix of C++ and Haskell and the second is Haskell only.

Download the latest htar9-asm-exe
[here](https://gitlab.com/BorisNikulin/htar9-asm/-/jobs/artifacts/master/raw/build/htar9-asm-exe?job=build-cpp).

Download the latest htar9-asm-hs-exe
[here](https://gitlab.com/BorisNikulin/htar9-asm/-/jobs/artifacts/master/raw/build/htar9-asm-hs-exe?job=build-hs).

## Documentation
The ISA specification can be found
[here](https://borisnikulin.gitlab.io/htar9-asm/isa.pdf).

## Reports
Test coverage report can be found by clicking on coverage badge or
[here](https://borisnikulin.gitlab.io/htar9-asm/coverage).

Benchmark report can be found
[here](https://borisnikulin.gitlab.io/htar9-asm/benchmark).
N.B. The benchmark report linked above was obtained from gitlab free runners, so benchmarking conditions are not ideal.
Take that report with a large grain of salt.
It is recommended to run the benchmarks locally and in *quite* conditions to obtain more meaningful benchmarks.

## Usage

### htar9-asm-hs-exe interpreter
Since the tui is in wip (although nearly feature complete) and there is no help for it,
here are the valid inputs:

* q to quit
* arrow keys, pg down/up, end/home, vi keys to move about the instruction list
(see [brick list doc](https://hackage.haskell.org/package/brick-0.35.1/docs/Brick-Widgets-List.html#v:handleListEventVi))
* s to step the interpreter once if not done
* b to toggle breakpoints for selected (underlined) instruction
* r to run to next breakpoint or until done
* i to send init signal to the cpu (sets done flag to 0 allowing to step/run past a fin instruction)
