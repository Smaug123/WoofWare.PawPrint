# native-call-graph

Writes, for each runtime `flake.nix` pins and each of Linux x64 and Linux arm64, the call graph of the C++ that
`WoofWare.PawPrint.Semantics/NativeContractTable.tsv` cites, as clang parses a release build of CoreCLR. The graphs
are checked in at `WoofWare.PawPrint.Test/NativeCallGraph/NativeCallGraph.<runtime>.<target>.tsv`, and the flake check
`native-call-graph-<runtime>-<target>` fails unless each is what this directory generates now. To regenerate one:

```bash
nix build .#native-call-graph-net10-linux-x64
cp result/NativeCallGraph.net10.linux-x64.tsv WoofWare.PawPrint.Test/NativeCallGraph/
```

## What a graph says

Its header names the runtime's commit, the target, the compiler and the configure flags. Then its rows, by their
first field:

* `routine KEY NAME SIGNATURE USR PATH:LINE`: a definition of a requested routine. A key is a citation as the
  contract table writes it in its sources, or a vouch's callee as `NativeContractCallees.tsv` writes it, where either
  names a file of C or C++. A definition matches a key when it is in the file the key names and its fully qualified
  name is the key's symbol or ends in `::` and the symbol, as the tables' own reader matches: `GCHeap::GetLOHThreshold`
  matches both `WKS::GCHeap` and `SVR::GCHeap`'s definitions in `gc/gc.cpp`. A template's key, which names it without
  arguments, matches each of its instantiations.
* `uninstantiated KEY`: the key names a template no unit instantiates.
* `undefined KEY`: no unit defines a function the key names. Most are macros.
* `call ROUTINE-NAME ROUTINE-SIGNATURE ROUTINE-USR PATH:LINE MACRO KIND CALLEE-NAME CALLEE-SIGNATURE CALLEE-USR ENUMS`:
  one call a routine's definition makes.

Names are qualified fully, signatures are canonical types, and paths are relative to the runtime's root, or to the
sysroot (`sysroot/`) or clang's resource directory (`clang/`). `PATH:LINE` is where a reader of the file sees the call:
at the outermost macro's invocation for a call a macro's body makes, and where it is written for one written in a
macro's argument. `MACRO` is `PATH:NAME` of that outermost macro, or `-`. `ENUMS` lists each argument naming an
enumerator, as `N=Enum::Enumerator`, numbering arguments from 1 and counting defaults.

The kinds of call:

* `direct`: to the function named, including a virtual one called with a qualifier (`X::F()`).
* `virtual`: one row for each final overrider of the method, in each class the program defines that is, or derives
  from, the receiver's static class and is not abstract. The program is every unit of `libcoreclr.so` and
  `libclrgc.so`, so a class that only another binary defines is not seen.
* `virtual-unimplemented`: a virtual call no class the program defines can take; the callee is the method named.
* `indirect`: through a pointer to a function or to a member function. The callee is `-`, and the signature the
  pointer's.
* `construct`, `destruct`: a constructor or destructor that is not trivial. Destructors are those of every automatic
  variable, every temporary, and in a constructor or destructor, every base and member; these are every destructor
  the CFG would add, and also those that unwinding a constructor runs.
* `new`, `delete`: the `operator new` or `operator delete` a `new` or `delete` calls. A `delete` of a class with a
  virtual destructor is also a `virtual` call of the destructor.
* `throw`: a `throw`, whose callee is the type thrown, or `-` for a rethrow.
* `asm`: inline assembly, whose calls, if any, are not seen.

Nothing in an unevaluated operand (`sizeof`, `decltype`, `noexcept`, a `typeid` of a type that is not
polymorphic), a constant expression, a `static_assert` or the discarded branch of an `if constexpr` is a call. A
default argument's or default member initializer's calls are made where it is used. A lambda's or a local class's
body counts as its enclosing routine's.

## How it is made

`default.nix` builds, for each runtime and target:

1. A sysroot of the target's glibc, libstdc++, lttng-ust and liburcu, from nixpkgs's packages for that target,
   substituted rather than built.
2. A CMake configure of `src/coreclr`, from the flake's sparse checkout with `coreclr-configure.patch` applied, using
   the runtime's own `eng/native/tryrun.cmake`, with clang and lld as the cross-compiler. `program.py` then generates
   the files the units read that the build itself writes, such as the eventing headers, and lists the units of the two
   libraries. Nothing is compiled.
3. `requests.py` reads the routines to describe from the two tables. `generate.py` runs `callgraph.cpp`, a LibTooling
   program, over every unit twice: for the routines' calls, and then for the overriders of each method those call
   virtually.

`oracle/` is a small program with every case above, and `oracle/expected.tsv` its graph, less its USRs; the flake check
`native-call-graph-oracle` holds the generator to it.
