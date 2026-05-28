# WoofWare.PawPrint

*Slop status: original architecture is by me, with only reference assistance from LLMs. Then in 2026 I drove GPT-5.5 and Claude Opus 4.6/4.7 hard to get this to a usable state. Architecture is still mine, and I've read all the incoming non-test code, but have perhaps been a bit sloppy about some of it.*

This is an *extremely unfinished* implementation of a .NET runtime (specifically .NET 10).

## Current status

The following work, at least in some minimal form:

* `Console.WriteLine`
* `async void Main(string[] args) { ... }`
* `Task.Run`
* Quite a lot of reflection
* Many low-level synchronisation primitives like `Monitor`

The following are specifically not implemented:

* GC and finalizers

## Goals

* Fully deterministic, ultimately to the point of supporting time-travel debugging and fuzzing over the order of thread execution. All sources of nondeterminism must be controllable by the PawPrint user somehow, such that emulating the same program twice from the same starting state always produces the same execution history.
* Fully managed. For example, I expect I will have to reimplement a *large* number of methods which are defined by P/Invoke, so that my deterministic runtime does not have to emulate native code.
* Fully in-memory except insofar as the program under test performs filesystem operations.

## Non-goals

* Performance. I expect this to be a *very slow* IL interpreter.
* Fidelity to the optimisations performed by e.g. the JIT or the GC. I am purely interpreting IL. For example, it is likely that I will simply never deallocate memory (so e.g. finalisers are not run).
* Support for any operating systems other than the ones on which I am running (currently macOS, but if this project actually starts working, then I will start using Linux).

## Licence

[MIT](./LICENCE.md).

This project was produced with reference to the .NET runtime, which was used under the [MIT licence](./LICENCE_dotnet.md).
WoofWare.PawPrint may contain small amounts of that code.
