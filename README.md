# WoofWare.PawPrint

This is an *extremely unfinished* implementation of a .NET runtime.

## Goals

* Fully deterministic, ultimately to the point of supporting time-travel debugging and fuzzing over the order of thread execution. All sources of nondeterminism must be controllable by the PawPrint user somehow, such that emulating the same program twice from the same starting state always produces the same execution history.
* Fully managed. For example, I expect I will have to reimplement a *large* number of methods which are defined by P/Invoke, so that my deterministic runtime does not have to emulate native code.
* Fully in-memory except insofar as the program under test performs filesystem operations.

## Non-goals

* Performance. I expect this to be a *very slow* IL interpreter.
* Fidelity to the optimisations performed by e.g. the JIT or the GC. I am purely interpreting IL. For example, it is likely that I will simply never deallocate memory (so e.g. finalisers are not run).
* Support for any operating systems other than the ones on which I am running (currently macOS, but if this project actually starts working, then I will start using Linux).
