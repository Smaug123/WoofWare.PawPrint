# WoofWare.PawPrint

## Slop status

Original architecture is by me, with only reference assistance from LLMs.
Then in early 2026 I drove GPT-5.5 and Claude Opus 4.6/4.7 hard to get this to a usable state, resulting in release 0.1.1; I did review all the code during that time.

Since then, as I wanted to use this to debug some real life flaky tests, I drove GPT-5.6 Sol and Claude Opus 4.8/5 and Fable 5 much harder, and gradually stopped reading the code.
As of August 2026, it would probably qualify as fully vibe-coded, although it *is* (astonishingly) a powerful and working tool which has actually debugged three real flaky tests in extremely nontrivial projects.

## Description

This is an *unfinished* deterministic implementation of a .NET runtime (specifically .NET 10).
You give it a DLL, and it executes the entry point therein on an emulated runtime which controls all sources of nondeterminism.

## Current project status

Even incomplete as it is, PawPrint is currently capable of automatically detecting and reproducing a number of textbook [race conditions](WoofWare.PawPrint.Test/sourcesConcurrencyBugs/), by running the input program with many different seeds for its source of randomness.

Nontrivial programs will probably fail loudly, unless you're lucky enough to be using only what I've already implemented.

The following work, at least in some minimal form:

* `Console.WriteLine`
* `async void Main(string[] args) { ... }`
* `Task.Run`
* Quite a lot of reflection
* Many low-level synchronisation primitives like `Monitor`

The following are specifically not implemented:

* GC and finalizers

## Getting started

See the [run-a-program.md](./docs/user/run-a-program.md) doc for how to run a program, and [fuzz-over-thread-scheduler.md](./docs/user/fuzz-over-thread-scheduler.md) for how to automatically detect concurrency-related bugs.

## Goals

* Fully deterministic, ultimately to the point of supporting time-travel debugging and fuzzing over the order of thread execution. All sources of nondeterminism must be controllable by the PawPrint user somehow, such that emulating the same program twice from the same starting state always produces the same execution history.
* Fully managed. For example, I reimplement a *large* number of methods which are defined by P/Invoke, so that my deterministic runtime does not have to emulate native code.
* Fully in-memory except insofar as the program under test performs filesystem operations. (Filesystem operations are not yet supported, although the stderr/stdout file descriptors are.)
* No monkey-patching stuff out for convenience. IL interpretation is faithful. We emulate a rather eccentric JIT (the BCL relies on the presence of a JIT!), but if it's not a JIT intrinsic and it's not native code, we execute its genuine implementation.

## Non-goals

* Performance. I expect this to be a *very slow* IL interpreter.
* Fidelity to the optimisations performed by e.g. RyuJIT or .NET's GC. I am purely interpreting IL (and mocking out native calls). For example, it is likely that I will simply never deallocate memory (so e.g. finalisers are not run).
* Support for any operating systems other than the ones on which I am running (currently macOS and Linux), and any fancy hardware features like SIMD.

## Correctness

The project aims for correctness over availability, and will happily crash whenever it doesn't recognise some situation.

An advantage of a .NET runtime as a project is that it's unusually easy to test, because the CLR is a reference implementation.
WoofWare.PawPrint has [quite a lot of tests](./WoofWare.PawPrint.Test/sourcesPure) that "some C# code has the same observable result under PawPrint and the CLR", and a couple of [F# tests](./WoofWare.PawPrint.Test.FSharpPureCases/Main.fs/) too.
(This makes me much happier about the rampant LLM usage than I would otherwise have been!)

## Licence

[MIT](./LICENCE.md).

This project was produced with reference to the .NET runtime, which was used under the [MIT licence](./LICENCE_dotnet.md).
WoofWare.PawPrint may contain small amounts of that code.
